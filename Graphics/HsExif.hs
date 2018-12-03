{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Ability to work with the EXIF data contained in image files.
module Graphics.HsExif (
    -- $intro

    -- * Main functions
    parseFileExif,
    parseExif,

    -- * Higher-level helper functions
    readExifDateTime,
    getDateTimeOriginal,
    getOrientation,
    ImageOrientation(..),
    RotationDirection(..),
    getGpsLatitudeLongitude,
    wasFlashFired,
    formatAsFloatingPoint,
    getGpsDateTime,
    parseGpsTime,

    -- * The ExifValue type
    ExifValue(..),

    -- * Most useful exif tags
    exposureTime,
    fnumber,
    isoSpeedRatings,
    dateTimeOriginal,
    shutterSpeedValue,
    apertureValue,
    brightnessValue,
    exposureBiasValue,
    maxApertureValue,
    flash,
    focalLength,
    userComment,
    orientation,
    make,
    model,
    software,
    copyright,
    digitalZoomRatio,
    focalLengthIn35mmFilm,
    artist,

    -- * GPS related exif tags

    gpsVersionID,
    gpsLatitudeRef,
    gpsLatitude,
    gpsLongitudeRef,
    gpsLongitude,
    gpsAltitudeRef,
    gpsAltitude,
    gpsTimeStamp,
    gpsSatellites,
    gpsStatus,
    gpsMeasureMode,
    gpsDop,
    gpsSpeedRef,
    gpsSpeed,
    gpsTrackRef,
    gpsTrack,
    gpsImgDirectionRef,
    gpsImgDirection,
    gpsMapDatum,
    gpsDestLatitudeRef,
    gpsDestLatitude,
    gpsDestLongitudeRef,
    gpsDestLongitude,
    gpsDestBearingRef,
    gpsDestBearing,
    gpsDestDistanceRef,
    gpsDestDistance,
    gpsProcessingMethod,
    gpsAreaInformation,
    gpsDateStamp,
    gpsDifferential,

    -- * Less useful exif tags

    exifVersion,
    sensingMethod,
    fileSource,
    sceneType,
    makerNote,
    subjectDistance,
    meteringMode,
    lightSource,
    exifImageWidth,
    exifImageHeight,
    relatedSoundFile,
    focalPlaneXResolution,
    focalPlaneYResolution,
    focalPlaneResolutionUnit,
    dateTimeDigitized,
    componentConfiguration,
    compressedBitsPerPixel,
    exposureProgram,
    spectralSensitivity,
    oecf,
    subjectArea,
    subSecTime,
    subSecTimeOriginal,
    subSecTimeDigitized,
    flashPixVersion,
    colorSpace,
    flashEnergy,
    spatialFrequencyResponse,
    subjectLocation,
    exposureIndex,
    cfaPattern,
    customRendered,
    exposureMode,
    whiteBalance,
    sceneCaptureType,
    gainControl,
    contrast,
    saturation,
    sharpness,
    deviceSettingDescription,
    subjectDistanceRange,
    imageUniqueId,
    exifInteroperabilityOffset,
    imageDescription,
    xResolution,
    yResolution,
    resolutionUnit,
    dateTime,
    whitePoint,
    primaryChromaticities,
    yCbCrPositioning,
    yCbCrCoefficients,
    referenceBlackWhite,
    printImageMatching,

    -- * If you need to declare your own exif tags
    ExifTag(..),
    TagLocation(..),
) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Control.Monad
import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Internal (w2c)
import Data.Word
import Data.Char (ord)
import Data.Int (Int32, Int16, Int8)
import Data.List
import Data.Maybe (fromMaybe, fromJust) -- TODO try to get rid of fromJust
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Bits ((.&.))
import Control.Exception
import System.IO

import Graphics.Types (ExifValue(..), ExifTag(..), TagLocation(..), formatAsFloatingPoint)
import Graphics.ExifTags
import Graphics.Helpers

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html
-- and http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
-- and http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html
-- and http://www.exiv2.org/tags.html
-- and https://libopenraw.freedesktop.org/wiki/Fuji_RAF/

-- | Read EXIF data from the file you give. It's a key-value map.
-- The reading is strict to avoid file handle exhaustion on a recursive
-- reading of a directory tree.
parseFileExif :: FilePath -> IO (Either String (Map ExifTag ExifValue))
parseFileExif filename = withFile filename ReadMode ((evaluate =<<) . fmap parseExif . B.hGetContents)

-- | Read EXIF data from a lazy bytestring.
parseExif :: B.ByteString -> Either String (Map ExifTag ExifValue)
parseExif = runEitherGet getExif

getExif :: Get (Map ExifTag ExifValue)
getExif = do
    firstBytes <- lookAhead $ (,) <$> getWord16be <*> getWord16be
    case firstBytes of
        (0xffd8,_ ) -> getWord16be >> findAndParseExifBlockJPEG
        (0x4d4d,0x002A) -> findAndParseExifBlockTiff  -- TIFF big-endian: DNG, Nikon
        (0x4949,0x2A00) -> findAndParseExifBlockTiff  -- TIFF little-endian: Canon CR2, Sony ARW
        -- The following formats use the TIFF structure, but use their
        -- own version number instead of 42.
        (0x4949,0x524F) -> findAndParseExifBlockTiff  -- Olympus ORF
        (0x4949,0x5500) -> findAndParseExifBlockTiff  -- Panasonic RW2
        -- Fuji RAF files use a custom format with an embedded JPEG preview containing the EXIF data
        (0x4655,0x4A49) -> findAndParseExifBlockFuji  -- Fuji RAF
        _           -> fail "Not a JPEG, TIFF, RAF, or TIFF-based raw file"

findAndParseExifBlockJPEG :: Get (Map ExifTag ExifValue)
findAndParseExifBlockJPEG = do
    markerNumber <- getWord16be
    dataSize <- fromIntegral . toInteger <$> getWord16be
    case markerNumber of
        0xffe1 -> parseExifBlock
        -- ffda is Start Of Stream => image
        -- I expect no more EXIF data after this point.
        0xffda -> fail "No EXIF in JPEG"
        _ -> skip (dataSize-2) >> findAndParseExifBlockJPEG

findAndParseExifBlockTiff :: Get (Map ExifTag ExifValue)
findAndParseExifBlockTiff = parseTiff

findAndParseExifBlockFuji :: Get (Map ExifTag ExifValue)
findAndParseExifBlockFuji = do
    header <- getByteString 16
    version <- getByteString 4
    skip 64
    jpegOffset <- getWord32be
    unless (header == "FUJIFILMCCD-RAW " && version == "0201") $ fail "Incorrect RAF header"
    skip $
      fromIntegral jpegOffset  -- skip to jpeg data (offset is from start of file)
      - 16                     -- 16 bytes for header
      - 4                      -- 4 bytes for version
      - 64                     -- 64 other bytes skiped
      - 4                      -- 4 bytes jpeg offset
      + 2                      -- findAndParseExifBlockJPEG expects 2 bytes skipped
    findAndParseExifBlockJPEG

data ByteAlign = Intel | Motorola deriving (Eq)

getWord16 :: ByteAlign -> Get Word16
getWord16 Intel = getWord16le
getWord16 Motorola = getWord16be

getWord32 :: ByteAlign -> Get Word32
getWord32 Intel = getWord32le
getWord32 Motorola = getWord32be

putWord32 :: ByteAlign -> Word32 -> Put
putWord32 Intel = putWord32le
putWord32 Motorola = putWord32be

parseExifBlock :: Get (Map ExifTag ExifValue)
parseExifBlock = do
    header <- getByteString 4
    nul <- toInteger <$> getWord16be
    unless (header == Char8.pack "Exif" && nul == 0)
        $ fail "invalid EXIF header"
    parseTiff

parseTiff :: Get (Map ExifTag ExifValue)
parseTiff = do
    (byteAlign, ifdOffset) <- lookAhead parseTiffHeader
    tags <- parseIfd byteAlign IFD0 ifdOffset
    return $ Map.fromList tags

parseTiffHeader :: Get (ByteAlign, Int)
parseTiffHeader = do
    byteAlignV <- Char8.unpack <$> getByteString 2
    byteAlign <- case byteAlignV of
        "II" -> return Intel
        "MM" -> return Motorola
        _ -> fail $ "Unknown byte alignment: " ++ byteAlignV
    alignControl <- toInteger <$> getWord16 byteAlign
    unless (alignControl == 0x2a || (byteAlign == Intel && (alignControl == 0x55 || alignControl == 0x4f52)))
        $ fail "exif byte alignment mismatch"
    ifdOffset <- fromIntegral . toInteger <$> getWord32 byteAlign
    return (byteAlign, ifdOffset)

-- | Parse an Image File Directory table
parseIfd :: ByteAlign -> TagLocation -> Int -> Get [(ExifTag, ExifValue)]
parseIfd byteAlign ifdId offset = do
    entries <- lookAhead $ do
        skip offset
        dirEntriesCount <- fromIntegral <$> getWord16 byteAlign
        replicateM dirEntriesCount (parseIfEntry byteAlign ifdId)
    
    concat <$> mapM (entryTags byteAlign) entries

-- | Convert IFD entries to tags, reading sub-IFDs
entryTags :: ByteAlign -> IfEntry -> Get [(ExifTag, ExifValue)]
entryTags _ (Tag tag parseValue) = parseValue >>= \value -> pure [(tag, value)]
entryTags byteAlign (SubIFD ifdId offset) = lookAhead (parseIfd byteAlign ifdId offset)

data IfEntry = Tag ExifTag (Get ExifValue) | SubIFD TagLocation Int

-- | Parse a single IFD entry
parseIfEntry :: ByteAlign -> TagLocation -> Get IfEntry
parseIfEntry byteAlign ifdId = do
    tagNumber <- getWord16 byteAlign
    format <- getWord16 byteAlign
    numComponents <- fromIntegral <$> getWord32 byteAlign
    content <- getWord32 byteAlign

    return $ case (ifdId, tagNumber) of
        (IFD0, 0x8769) -> SubIFD ExifSubIFD (fromIntegral content)
        (IFD0, 0x8825) -> SubIFD GpsSubIFD  (fromIntegral content)
        (_, tagId)     -> Tag (getExifTag ifdId tagId) (decodeEntry byteAlign format numComponents content)

getExifTag :: TagLocation -> Word16 -> ExifTag
getExifTag l v = fromMaybe (ExifTag l Nothing v showT) $ find (isSameTag l v) allExifTags
    where isSameTag l1 v1 (ExifTag l2 _ v2 _) = l1 == l2 && v1 == v2

data ValueHandler = ValueHandler
    {
        dataTypeId :: Word16,
        dataLength :: Int,
        readSingle :: ByteAlign -> Get ExifValue,
        readMany :: ByteAlign -> Int -> Get ExifValue
    }

readNumberList :: Integral a => (ByteAlign -> Get a) -> ByteAlign -> Int -> Get ExifValue
readNumberList decoder byteAlign components = ExifNumberList . fmap fromIntegral <$>
            count components (decoder byteAlign)

decodeTextByteString :: BS.ByteString -> String
decodeTextByteString bs = w2c <$> strippedWords
    where
      strippedWords = if not (null bsWords) && last bsWords == 0
                      then init bsWords
                      else bsWords
      bsWords = BS.unpack bs

unsignedByteValueHandler = ValueHandler
    {
        dataTypeId = 1,
        dataLength = 1,
        readSingle = \_ -> ExifNumber . fromIntegral <$> getWord8,
        readMany = readNumberList $ const getWord8
    }

asciiStringValueHandler = ValueHandler
    {
        dataTypeId = 2,
        dataLength = 1,
        readSingle = \ba -> readMany asciiStringValueHandler ba 1,
        readMany = \_ components -> ExifText . decodeTextByteString <$> getByteString components
    }

unsignedShortValueHandler = ValueHandler
    {
        dataTypeId = 3,
        dataLength = 2,
        readSingle = liftM (ExifNumber . fromIntegral) . getWord16,
        readMany = readNumberList getWord16
    }

unsignedLongValueHandler = ValueHandler
    {
        dataTypeId = 4,
        dataLength = 4,
        readSingle = liftM (ExifNumber . fromIntegral) . getWord32,
        readMany = readNumberList getWord32
    }

readRationalContents :: (Int -> Int -> a) -> ByteAlign -> Get a
readRationalContents c byteAlign = do
    numerator   <- fromIntegral <$> getWord32 byteAlign
    denominator <- fromIntegral <$> getWord32 byteAlign
    return $ c numerator denominator

unsignedRationalValueHandler = ValueHandler
    {
        dataTypeId = 5,
        dataLength = 8,
        readSingle = readRationalContents ExifRational,
        readMany = \byteAlign components -> ExifRationalList <$> count components (readRationalContents (,) byteAlign)
    }

signedByteValueHandler = ValueHandler
    {
        dataTypeId = 6,
        dataLength = 1,
        readSingle = \_ -> ExifNumber . signedInt8ToInt <$> getWord8,
        readMany = readNumberList (liftM signedInt8ToInt . const getWord8)
    }

undefinedValueHandler = ValueHandler
    {
        dataTypeId = 7,
        dataLength = 1,
        readSingle = \ba -> readMany undefinedValueHandler ba 1,
        readMany = \_ components -> ExifUndefined <$> getByteString components
    }

signedShortValueHandler = ValueHandler
    {
        dataTypeId = 8,
        dataLength = 2,
        readSingle = liftM (ExifNumber . signedInt16ToInt) . getWord16,
        readMany = readNumberList (liftM signedInt16ToInt . getWord16)
    }

signedLongValueHandler = ValueHandler
    {
        dataTypeId = 9,
        dataLength = 4,
        readSingle = liftM (ExifNumber . signedInt32ToInt) . getWord32,
        readMany = readNumberList (liftM signedInt32ToInt . getWord32)
    }

readSignedRationalContents :: (Int -> Int -> a) -> ByteAlign -> Get a
readSignedRationalContents c byteAlign = do
    numerator   <- signedInt32ToInt <$> getWord32 byteAlign
    denominator <- signedInt32ToInt <$> getWord32 byteAlign
    return $ c numerator denominator

signedRationalValueHandler = ValueHandler
    {
        dataTypeId = 10,
        dataLength = 8,
        readSingle = readSignedRationalContents ExifRational,
        readMany = \byteAlign components -> ExifRationalList <$>
            count components (readSignedRationalContents (,) byteAlign)
    }

valueHandlers :: [ValueHandler]
valueHandlers =
    [
        unsignedByteValueHandler,
        asciiStringValueHandler,
        unsignedShortValueHandler,
        unsignedLongValueHandler,
        unsignedRationalValueHandler,
        signedByteValueHandler,
        signedShortValueHandler,
        signedLongValueHandler,
        signedRationalValueHandler,
        undefinedValueHandler
    ]

decodeEntry :: ByteAlign -> Word16 -> Int -> Word32 -> Get ExifValue
decodeEntry byteAlign format amount payload = do
    case getHandler format of
        Just handler | isInline handler -> return $ parseInline byteAlign handler amount (runPut $ putWord32 byteAlign payload)
        Just handler                    -> parseOffset byteAlign handler amount payload
        Nothing -> return $ ExifUnknown format amount (fromIntegral payload)
    where
        isInline handler = dataLength handler * amount <= 4

getHandler :: Word16 -> Maybe ValueHandler
getHandler typeId = find ((==typeId) . dataTypeId) valueHandlers

parseInline :: ByteAlign -> ValueHandler -> Int -> B.ByteString -> ExifValue
parseInline byteAlign handler amount bytestring =
    fromJust $ runMaybeGet getter bytestring
    where
        getter = case amount of
            1 -> readSingle handler byteAlign
            _ -> readMany handler byteAlign amount

parseOffset :: ByteAlign -> ValueHandler -> Int -> Word32 -> Get ExifValue
parseOffset byteAlign handler amount offset = do
    -- this skip can take me quite far and I can't skip
    -- back with binary. So do the skip with a lookAhead.
    -- see https://github.com/emmanueltouzery/hsexif/issues/9
    lookAhead $ do
        skip (fromIntegral offset)
        let bsLength = amount * dataLength handler
        bytestring <- getLazyByteString (fromIntegral bsLength)
        return (parseInline byteAlign handler amount bytestring)

signedInt32ToInt :: Word32 -> Int
signedInt32ToInt w = fromIntegral (fromIntegral w :: Int32)

signedInt16ToInt :: Word16 -> Int
signedInt16ToInt w = fromIntegral (fromIntegral w :: Int16)

signedInt8ToInt :: Word8 -> Int
signedInt8ToInt w = fromIntegral (fromIntegral w :: Int8)

-- | Decode an EXIF date time value.
-- Will return 'Nothing' in case parsing fails.
readExifDateTime :: String -> Maybe LocalTime
readExifDateTime dateStr = runMaybeGet getExifDateTime $ B.pack $ map (fromIntegral . ord) dateStr

getExifDateTime :: Get LocalTime
getExifDateTime = do
    year   <- readDigit 4
    month  <- getCharValue ':' >> readDigit 2
    day    <- getCharValue ':' >> readDigit 2
    hour   <- getCharValue ' ' >> readDigit 2
    minute <- getCharValue ':' >> readDigit 2
    second <- getCharValue ':' >> readDigit 2
    return $ LocalTime (fromGregorian year month day) (TimeOfDay hour minute second)

-- | Extract the date and time when the picture was taken
-- from the EXIF information.
getDateTimeOriginal :: Map ExifTag ExifValue -> Maybe LocalTime
getDateTimeOriginal exifData = Map.lookup dateTimeOriginal exifData >>= readExifDateTime . show

data RotationDirection = MinusNinety
    | Ninety
    | HundredAndEighty
    deriving (Show, Eq)

data ImageOrientation = Normal
    | Mirror
    | Rotation RotationDirection
    | MirrorRotation RotationDirection
    deriving (Show, Eq)

-- | Extract the image orientation from the EXIF information.
-- Will return 'Nothing' on parse error.
getOrientation :: Map ExifTag ExifValue -> Maybe ImageOrientation
getOrientation exifData = do
    rotationVal <- Map.lookup orientation exifData
    case rotationVal of
        ExifNumber 1 -> Just Normal
        ExifNumber 2 -> Just Mirror
        ExifNumber 3 -> Just $ Rotation HundredAndEighty
        ExifNumber 4 -> Just $ MirrorRotation HundredAndEighty
        ExifNumber 5 -> Just $ MirrorRotation MinusNinety
        ExifNumber 6 -> Just $ Rotation MinusNinety
        ExifNumber 7 -> Just $ MirrorRotation Ninety
        ExifNumber 8 -> Just $ Rotation Ninety
        _ -> Nothing

-- | Will return Just True if the flash was fired, Just False
-- if it was not, and Nothing if the file does not contain
-- the information.
wasFlashFired :: Map ExifTag ExifValue -> Maybe Bool
wasFlashFired exifData = Map.lookup flash exifData >>= \case
    ExifNumber n -> Just $ n .&. 1 /= 0
    _ -> Nothing

-- $intro
--
-- EXIF parsing from JPEG and some RAW files.
-- EXIF tags are enumerated as ExifTag values, check 'exposureTime' for instance.
-- If you use the predefined ExifTag values, you don't care about details
-- of the ExifTag type, however you should check out the 'ExifValue' type.
--
-- Regarding the ExifTag type there is however a field of that type that may
-- interest you: 'prettyPrinter'. It's a function that'll format nicely an exif value
-- for that exif tag as a String.
-- For instance for the 'flash' ExifTag, it'll say whether the flash was
-- fired or not, if there was return light and so on.
--
-- Generally speaking, you start from a JPEG file, you can parse its exif tags as a 'Map' of
-- 'ExifTag' to 'ExifValue' using 'parseExif' or 'parseFileExif'.
-- You can enumerate the map or 'lookup' the tags that interest you.
--
-- There are also a couple of higher-level helpers like 'getOrientation',
-- 'getDateTimeOriginal', 'wasFlashFired' and 'getGpsLatitudeLongitude'.
--
-- When building on Windows if you have trouble with the @iconv@ library,
-- you may build without that dependency: @cabal install -f-iconv@.
-- That way you loose nice decoding of the EXIF User Comment though.
