{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Ability to work with the EXIF data contained in JPEG files.
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
	exifIfdOffset,
	printImageMatching,
	gpsTagOffset,

	-- * If you need to declare your own exif tags
	ExifTag(..),
	TagLocation(..),
) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Control.Monad (liftM, unless, replicateM)
import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Char8 as Char8
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

import Graphics.Types (ExifValue(..), ExifTag(..), TagLocation(..), formatAsFloatingPoint)
import Graphics.ExifTags
import Graphics.Helpers

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html
-- and http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
-- and http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html
-- and http://www.exiv2.org/tags.html

-- | Read EXIF data from the file you give. It's a key-value map.
parseFileExif :: FilePath -> IO (Either String (Map ExifTag ExifValue))
parseFileExif filename = parseExif <$> B.readFile filename

-- | Read EXIF data from a lazy bytestring.
parseExif :: B.ByteString -> Either String (Map ExifTag ExifValue)
parseExif = runEitherGet getExif

getExif :: Get (Map ExifTag ExifValue)
getExif = do
	header <- getWord16be
	unless (header == 0xffd8)
		$ fail "Not a JPEG file"
	findAndParseExifBlock

findAndParseExifBlock :: Get (Map ExifTag ExifValue)
findAndParseExifBlock = do
	markerNumber <- getWord16be
	dataSize <- fromIntegral . toInteger <$> getWord16be
	case markerNumber of
		0xffe1 -> parseExifBlock
		-- ffda is Start Of Stream => image
		-- I expect no more EXIF data after this point.
		0xffda -> fail "No EXIF in JPEG" 
		_ -> skip (dataSize-2) >> findAndParseExifBlock

data ByteAlign = Intel | Motorola

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
	tiffHeaderStart <- fromIntegral <$> bytesRead
	byteAlign <- parseTiffHeader
	let subIfdParse = parseSubIFD byteAlign tiffHeaderStart
	(mExifSubIfdOffsetW, mGpsOffsetW, ifdEntries) <- parseIfd byteAlign tiffHeaderStart
	gpsData <- maybe (return []) (lookAhead . subIfdParse GpsSubIFD) mGpsOffsetW
	exifSubEntries <- maybe (return []) (subIfdParse ExifSubIFD) mExifSubIfdOffsetW
	return $ Map.fromList $ ifdEntries ++ exifSubEntries ++ gpsData

parseSubIFD :: ByteAlign -> Int -> TagLocation -> Word32 -> Get [(ExifTag, ExifValue)]
parseSubIFD byteAlign tiffHeaderStart ifdType offsetW = do
	let offset = fromIntegral $ toInteger offsetW
	bytesReadNow <- fromIntegral <$> bytesRead
	skip $ (offset + tiffHeaderStart) - bytesReadNow
	parseSubIfd byteAlign tiffHeaderStart ifdType

parseTiffHeader :: Get ByteAlign
parseTiffHeader = do
	byteAlignV <- Char8.unpack <$> getByteString 2
	byteAlign <- case byteAlignV of
		"II" -> return Intel
		"MM" -> return Motorola
		_ -> fail $ "Unknown byte alignment: " ++ byteAlignV
	alignControl <- toInteger <$> getWord16 byteAlign
	unless (alignControl == 0x2a)
		$ fail "exif byte alignment mismatch"
	ifdOffset <- fromIntegral . toInteger <$> getWord32 byteAlign
	skip $ ifdOffset - 8
	return byteAlign

parseIfd :: ByteAlign -> Int -> Get (Maybe Word32, Maybe Word32, [(ExifTag, ExifValue)])
parseIfd byteAlign tiffHeaderStart = do
	dirEntriesCount <- fromIntegral <$> getWord16 byteAlign
	ifdEntries <- replicateM dirEntriesCount (parseIfEntry byteAlign)
	let exifOffset = entryContentsByTag exifIfdOffset ifdEntries
	let gpsOffset = entryContentsByTag gpsTagOffset ifdEntries
	entries <- mapM (decodeEntry byteAlign tiffHeaderStart IFD0) ifdEntries
	return (exifOffset, gpsOffset, entries)

entryContentsByTag :: ExifTag -> [IfEntry] -> Maybe Word32
entryContentsByTag tag = fmap entryContents . find (\e -> entryTag e == tagKey tag)

parseSubIfd :: ByteAlign -> Int -> TagLocation -> Get [(ExifTag, ExifValue)]
parseSubIfd byteAlign tiffHeaderStart location = do
	dirEntriesCount <- fromIntegral <$> getWord16 byteAlign
	ifdEntries <- replicateM dirEntriesCount (parseIfEntry byteAlign)
	mapM (decodeEntry byteAlign tiffHeaderStart location) ifdEntries

data IfEntry = IfEntry
	{
		entryTag :: !Word16,
		entryFormat :: !Word16,
		entryNoComponents :: !Int,
		entryContents :: !Word32
	} deriving Show

parseIfEntry :: ByteAlign -> Get IfEntry
parseIfEntry byteAlign = do
	tagNumber <- getWord16 byteAlign
	dataFormat <- getWord16 byteAlign
	numComponents <- getWord32 byteAlign
	value <- getWord32 byteAlign
	return IfEntry
		{
			entryTag = tagNumber,
			entryFormat = dataFormat,
			entryNoComponents = fromIntegral $ toInteger numComponents,
			entryContents = value
		}

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
		readMany = \_ components -> ExifText . Char8.unpack <$> getByteString (components-1)
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
	numerator <- fromIntegral <$> getWord32 byteAlign
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
	numerator <- signedInt32ToInt <$> getWord32 byteAlign
	denominator <- signedInt32ToInt <$> getWord32 byteAlign
	return $ c numerator denominator

signedRationalValueHandler = ValueHandler
	{
		dataTypeId = 10,
		dataLength = 8,
		readSingle = readSignedRationalContents ExifRational,
		readMany = \byteAlign components -> ExifRationalList <$> count components (readSignedRationalContents (,) byteAlign)
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

decodeEntry :: ByteAlign -> Int -> TagLocation -> IfEntry -> Get (ExifTag, ExifValue)
decodeEntry byteAlign tiffHeaderStart location entry = do
	let exifTag = getExifTag location $ entryTag entry
	let contentsInt = fromIntegral $ toInteger $ entryContents entry
	-- because I only know how to skip ahead, I hope the entries
	-- are always sorted in order of the offsets to their values...
	-- (maybe lookAhead could help here?)
	tagValue <- case getHandler $ entryFormat entry of
			Just handler -> decodeEntryWithHandler byteAlign tiffHeaderStart handler entry
			Nothing -> return $ ExifUnknown (entryFormat entry) (entryNoComponents entry) contentsInt
	return (exifTag, tagValue)

getHandler :: Word16 -> Maybe ValueHandler
getHandler typeId = find ((==typeId) . dataTypeId) valueHandlers

decodeEntryWithHandler :: ByteAlign -> Int -> ValueHandler -> IfEntry -> Get ExifValue
decodeEntryWithHandler byteAlign tiffHeaderStart handler entry =
	if dataLength handler * entryNoComponents entry <= 4
		then do
			let inlineBs = runPut $ putWord32 byteAlign $ entryContents entry
			return $ parseInline byteAlign handler entry inlineBs
		else parseOffset byteAlign tiffHeaderStart handler entry

parseInline :: ByteAlign -> ValueHandler -> IfEntry -> B.ByteString -> ExifValue
parseInline byteAlign handler entry bytestring =
	fromJust $ runMaybeGet getter bytestring
	where
		getter = case entryNoComponents entry of
			1 -> readSingle handler byteAlign
			_ -> readMany handler byteAlign $ entryNoComponents entry

parseOffset :: ByteAlign -> Int -> ValueHandler -> IfEntry -> Get ExifValue
parseOffset byteAlign tiffHeaderStart handler entry = do
	let contentsInt = fromIntegral $ toInteger $ entryContents entry
	curPos <- fromIntegral <$> bytesRead
	skip $ contentsInt + tiffHeaderStart - curPos
	bytestring <- getLazyByteString (fromIntegral $ entryNoComponents entry * dataLength handler)
	return $ parseInline byteAlign handler entry bytestring

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
	year <- readDigit 4
	month <- getCharValue ':' >> readDigit 2
	day <- getCharValue ':' >> readDigit 2
	hour <- getCharValue ' ' >> readDigit 2
	minute <- getCharValue ':' >> readDigit 2
	second <- getCharValue ':' >> readDigit 2
	-- the realToFrac is workaround for a GHC 7.8.0->7.8.2 bug:
	-- https://ghc.haskell.org/trac/ghc/ticket/9231
	-- actually generates a warning and it's good, reminds me to
	-- remove it someday, since 7.8.3 is out and with the fix.
	return $ LocalTime (fromGregorian year month day) (TimeOfDay hour minute $ realToFrac second)
	
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
wasFlashFired exifData = do
	Map.lookup flash exifData >>= \case
		ExifNumber n -> Just $ n .&. 1 /= 0
		_ -> Nothing

-- $intro
--
-- EXIF parsing from JPEG files.
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
