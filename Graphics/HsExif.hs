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
	formatAsFloatingPoint,

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
import qualified Data.ByteString as BS
import Control.Monad (liftM, unless, replicateM)
import qualified Data.ByteString.Char8 as Char8
import Data.Word
import Data.Char (isDigit, ord, chr)
import Data.Int (Int32, Int16, Int8)
import Data.List
import Data.Maybe (fromMaybe, fromJust) -- TODO try to get rid of fromJust
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Numeric (showHex)
import Text.Printf

-- | An exif value.
-- 
-- If you want a string describing the contents
-- of the value, simply use 'show'.
data ExifValue = ExifNumber !Int
	-- ^ An exif number. Originally it could have been short, int, signed or not.
	| ExifText !String
	-- ^ ASCII text.
	| ExifRational !Int !Int
	-- ^ A rational number (numerator, denominator).
	-- Sometimes we're used to it as rational (exposition time: 1/160),
	-- sometimes as float (exposure compensation, we rather think -0.75)
	-- 'show' will display it as 1/160.
	| ExifNumberList ![Int]
	-- ^ List of numbers. Originally they could have been short, int, signed or not.
	| ExifRationalList ![(Int,Int)]
	-- ^ A list of rational numbers (numerator, denominator).
	-- Sometimes we're used to it as rational (exposition time: 1/160),
	-- sometimes as float (exposure compensation, we rather think -0.75)
	-- 'show' will display it as 1/160.
	| ExifUndefined !BS.ByteString
	-- ^ The undefined type in EXIF means that the contents are not
	-- specified and up to the manufacturer. In effect it's exactly
	-- a bytestring. Sometimes it's text with ASCII or UNICODE at the
	-- beginning, often it's binary in nature.
	| ExifUnknown !Word16 !Int !Int
	-- ^ Unknown exif value type. All EXIF 2.3 types are
	-- supported, it could be a newer file.
	-- The parameters are type, count then value
	deriving Eq

instance Show ExifValue where
	show (ExifNumber v) = show v
	show (ExifText v) = v
	show (ExifRational n d) = show n ++ "/" ++ show d
	show (ExifUnknown t c v) = show "Unknown exif type. Type: " ++ show t 
		++ " count: " ++ show c ++ " value: " ++ show v
	show (ExifNumberList l) = show l
	show (ExifRationalList l) = show l
	show (ExifUndefined bs) = show bs

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html
-- and http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
-- and http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html
-- and http://www.exiv2.org/tags.html

-- | Read EXIF data from the file you give. It's a key-value map.
parseFileExif :: FilePath -> IO (Either String (Map ExifTag ExifValue))
parseFileExif filename = liftM parseExif $ B.readFile filename

-- | Read EXIF data from a lazy bytestring.
parseExif :: B.ByteString -> Either String (Map ExifTag ExifValue)
parseExif contents = case runGetOrFail getExif contents of
		Left (_,_,errorMsg) -> Left errorMsg
		Right (_,_,result) -> Right result

getExif :: Get (Map ExifTag ExifValue)
getExif = do
	header <- getWord16be
	unless (header == 0xffd8)
		$ fail "Not a JPEG file"
	findAndParseExifBlock

findAndParseExifBlock :: Get (Map ExifTag ExifValue)
findAndParseExifBlock = do
	markerNumber <- getWord16be
	dataSize <- liftM (fromIntegral . toInteger) getWord16be
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
	nul <- liftM toInteger getWord16be
	unless (header == Char8.pack "Exif" && nul == 0)
		$ fail "invalid EXIF header"
	tiffHeaderStart <- liftM fromIntegral bytesRead
	byteAlign <- parseTiffHeader
	(exifSubIfdOffsetW, mGpsOffsetW, ifdEntries) <- parseIfd byteAlign tiffHeaderStart
	let exifSubIfdOffset = fromIntegral $ toInteger exifSubIfdOffsetW
	gpsData <- case mGpsOffsetW of
		Nothing -> return []
		Just gpsOffsetW -> lookAhead $ parseGps gpsOffsetW byteAlign tiffHeaderStart
	-- skip to the exif offset
	bytesReadNow <- liftM fromIntegral bytesRead
	skip $ (exifSubIfdOffset + tiffHeaderStart) - bytesReadNow
	exifSubEntries <- parseSubIfd byteAlign tiffHeaderStart ExifSubIFD
	return $ Map.fromList $ ifdEntries ++ exifSubEntries ++ gpsData

parseGps :: Word32 -> ByteAlign -> Int -> Get [(ExifTag, ExifValue)]
parseGps gpsOffsetW byteAlign tiffHeaderStart = do
	let gpsOffset = fromIntegral $ toInteger gpsOffsetW
	bytesReadNow <- liftM fromIntegral bytesRead
	skip $ (gpsOffset + tiffHeaderStart) - bytesReadNow
	parseSubIfd byteAlign tiffHeaderStart GpsSubIFD

parseTiffHeader :: Get ByteAlign
parseTiffHeader = do
	byteAlignV <- getByteString 2
	let byteAlign = case Char8.unpack byteAlignV of
		"II" -> Intel
		"MM" -> Motorola
		_ -> error "Unknown byte alignment"
	alignControl <- liftM toInteger (getWord16 byteAlign)
	unless (alignControl == 0x2a)
		$ fail "exif byte alignment mismatch"
	ifdOffset <- liftM (fromIntegral . toInteger) (getWord32 byteAlign)
	skip $ ifdOffset - 8
	return byteAlign

parseIfd :: ByteAlign -> Int -> Get (Word32, Maybe Word32, [(ExifTag, ExifValue)])
parseIfd byteAlign tiffHeaderStart = do
	dirEntriesCount <- liftM toInteger (getWord16 byteAlign)
	ifdEntries <- mapM (\_ -> parseIfEntry byteAlign) [1..dirEntriesCount]
	let exifOffsetEntry = fromMaybe (error "Can't find the exif ifd offset")
		(find (\ e -> entryTag e == tagKey exifIfdOffset) ifdEntries)
	let exifOffset = entryContents exifOffsetEntry

	let gpsOffsetEntry = find (\ e -> entryTag e == tagKey gpsTagOffset) ifdEntries
	let gpsOffset = fmap entryContents gpsOffsetEntry
	entries <- mapM (decodeEntry byteAlign tiffHeaderStart IFD0) ifdEntries
	return (exifOffset, gpsOffset, entries)

parseSubIfd :: ByteAlign -> Int -> TagLocation -> Get [(ExifTag, ExifValue)]
parseSubIfd byteAlign tiffHeaderStart location = do
	dirEntriesCount <- liftM toInteger (getWord16 byteAlign)
	ifdEntries <- mapM (\_ -> parseIfEntry byteAlign) [1..dirEntriesCount]
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

-- | Location of the tag in the JPG file structure.
-- Normally you don't need to fiddle with this,
-- except maybe if the library doesn't know the particular
-- exif tag you're interested in.
-- Rather check the list of supported exif tags, like
-- 'exposureTime' and so on.
data TagLocation = ExifSubIFD | IFD0 | GpsSubIFD
	deriving (Show, Eq, Ord)

-- | An exif tag. Normally you don't need to fiddle with this,
-- except maybe if the library doesn't know the particular
-- exif tag you're interested in.
-- Rather check the list of supported exif tags, like
-- 'exposureTime' and so on.
data ExifTag = ExifTag
	{
		tagLocation :: TagLocation,
		-- ^ In which part of the JPEG file the exif tag was found
		tagDesc :: Maybe String,
		-- ^ Description of the exif tag (exposureTime, fnumber...) or if unknown, Nothing.
		-- This should be purely for debugging purposes, to compare tags use == on ExifTag
		-- or compare the tagKey.
		tagKey :: Word16
		-- ^ Exif tag key, the number uniquely identifying this tag.
	}
	deriving (Eq, Ord)

instance Show ExifTag where
	show (ExifTag _ (Just d) _) = d
	show (ExifTag l _ v) = "Unknown tag, location: " ++ show l
		++ ", value: 0x" ++ showHex v ""

exifSubIfdTag :: String -> Word16 -> ExifTag
exifSubIfdTag d = ExifTag ExifSubIFD (Just d)

exifIfd0Tag :: String -> Word16 -> ExifTag
exifIfd0Tag d = ExifTag IFD0 (Just d)

exifGpsTag :: String -> Word16 -> ExifTag
exifGpsTag d = ExifTag GpsSubIFD (Just d)

exposureTime		= exifSubIfdTag "exposureTime" 0x829a
fnumber			= exifSubIfdTag "fnumber" 0x829d
exposureProgram		= exifSubIfdTag "exposureProgram" 0x8822
spectralSensitivity	= exifSubIfdTag "spectralSensitivity" 0x8824
isoSpeedRatings		= exifSubIfdTag "isoSpeedRatings" 0x8827
oecf			= exifSubIfdTag "OECF" 0x8828
exifVersion		= exifSubIfdTag "exifVersion" 0x9000
dateTimeOriginal	= exifSubIfdTag "dateTimeOriginal" 0x9003
dateTimeDigitized	= exifSubIfdTag "dateTimeDigitized" 0x9004
componentConfiguration	= exifSubIfdTag "componentConfiguration" 0x9101
compressedBitsPerPixel	= exifSubIfdTag "compressedBitsPerPixel" 0x9102
shutterSpeedValue	= exifSubIfdTag "shutterSpeedValue" 0x9201
apertureValue		= exifSubIfdTag "apertureValue" 0x9202
brightnessValue		= exifSubIfdTag "brightnessValue" 0x9203
exposureBiasValue	= exifSubIfdTag "exposureBiasValue" 0x9204
maxApertureValue	= exifSubIfdTag "maxApertureValue" 0x9205
subjectDistance		= exifSubIfdTag "subjectDistance" 0x9206
meteringMode		= exifSubIfdTag "meteringMode" 0x9207
lightSource		= exifSubIfdTag "lightSource" 0x9208
flash			= exifSubIfdTag "flash" 0x9209
focalLength		= exifSubIfdTag "focalLength" 0x920a
subjectArea		= exifSubIfdTag "subjectArea" 0x9214
makerNote		= exifSubIfdTag "makerNote" 0x927c
userComment		= exifSubIfdTag "userComment" 0x9286
subSecTime		= exifSubIfdTag "subSecTime" 0x9290
subSecTimeOriginal	= exifSubIfdTag "subSecTimeOriginal" 0x9291
subSecTimeDigitized	= exifSubIfdTag "subSecTimeDigitized" 0x9292
flashPixVersion		= exifSubIfdTag "flashPixVersion" 0xa000
colorSpace		= exifSubIfdTag "colorSpace" 0xa001
exifImageWidth		= exifSubIfdTag "exifImageWidth" 0xa002
exifImageHeight		= exifSubIfdTag "exifImageHeight" 0xa003
relatedSoundFile	= exifSubIfdTag "relatedSoundFile" 0xa004
flashEnergy		= exifSubIfdTag "flashEnergy" 0xa20b
spatialFrequencyResponse= exifSubIfdTag "spatialFrequencyResponse" 0xa20c
focalPlaneXResolution	= exifSubIfdTag "focalPlaneXResolution" 0xa20e
focalPlaneYResolution	= exifSubIfdTag "focalPlaneYResolution" 0xa20f
focalPlaneResolutionUnit= exifSubIfdTag "focalPlaneResolutionUnit" 0xa210
subjectLocation		= exifSubIfdTag "subjectLocation" 0xa214
exposureIndex		= exifSubIfdTag "exposureIndex" 0xa215
sensingMethod		= exifSubIfdTag "sensingMethod" 0xa217
fileSource		= exifSubIfdTag "fileSource" 0xa300
sceneType		= exifSubIfdTag "sceneType" 0xa301
cfaPattern		= exifSubIfdTag "cfaPattern" 0xa302
customRendered		= exifSubIfdTag "customRendered" 0xa401
exposureMode		= exifSubIfdTag "exposureMode" 0xa402
whiteBalance		= exifSubIfdTag "whiteBalance" 0xa403
digitalZoomRatio	= exifSubIfdTag "digitalZoomRatio" 0xa404
focalLengthIn35mmFilm	= exifSubIfdTag "focalLengthIn35mmFilm" 0xa405
sceneCaptureType	= exifSubIfdTag "sceneCaptureType" 0xa406
gainControl		= exifSubIfdTag "gainControl" 0xa407
contrast		= exifSubIfdTag "contrast" 0xa408
saturation		= exifSubIfdTag "saturation" 0xa409
sharpness		= exifSubIfdTag "sharpness" 0xa40a
deviceSettingDescription= exifSubIfdTag "deviceSettingDescription" 0xa40b
subjectDistanceRange	= exifSubIfdTag "subjectDistanceRange" 0xa40c
imageUniqueId		= exifSubIfdTag "imageUniqueId" 0xa420
exifInteroperabilityOffset=exifSubIfdTag "exifInteroperabilityOffset" 0xa005

imageDescription	= exifIfd0Tag "imageDescription" 0x010e
make			= exifIfd0Tag "make" 0x010f
model			= exifIfd0Tag "model" 0x0110
orientation		= exifIfd0Tag "orientation" 0x0112
xResolution		= exifIfd0Tag "xResolution" 0x011a
yResolution		= exifIfd0Tag "xResolution" 0x011b
resolutionUnit		= exifIfd0Tag "resolutionUnit" 0x0128
software		= exifIfd0Tag "software" 0x0131
dateTime		= exifIfd0Tag "dateTime" 0x0132
artist			= exifIfd0Tag "artist" 0x013b
whitePoint		= exifIfd0Tag "whitePoint" 0x013e
primaryChromaticities	= exifIfd0Tag "primaryChromaticities" 0x013f
yCbCrCoefficients	= exifIfd0Tag "yCbCrCoefficients" 0x0211
yCbCrPositioning	= exifIfd0Tag "yCbCrPositioning" 0x0213
referenceBlackWhite	= exifIfd0Tag "referenceBlackWhite" 0x0214
copyright		= exifIfd0Tag "copyright" 0x8298
exifIfdOffset		= exifIfd0Tag "exifIfdOffset" 0x8769
gpsTagOffset		= exifIfd0Tag "gpsTagOffset" 0x8825
printImageMatching	= exifIfd0Tag "printImageMatching" 0xc4a5

gpsVersionID		= exifGpsTag "gpsVersionID" 0x0000
gpsLatitudeRef		= exifGpsTag "gpsLatitudeRef" 0x0001
gpsLatitude		= exifGpsTag "gpsLatitude" 0x0002
gpsLongitudeRef		= exifGpsTag "gpsLongitudeRef" 0x0003
gpsLongitude		= exifGpsTag "gpsLongitude" 0x0004
gpsAltitudeRef		= exifGpsTag "gpsAltitudeRef" 0x0005
gpsAltitude		= exifGpsTag "gpsAltitude" 0x0006
gpsTimeStamp		= exifGpsTag "gpsTimeStamp" 0x0007
gpsSatellites		= exifGpsTag "gpsSatellites" 0x0008
gpsStatus		= exifGpsTag "gpsStatus" 0x0009
gpsMeasureMode		= exifGpsTag "gpsMeasureMode" 0x000a
gpsDop			= exifGpsTag "gpsDop" 0x000b
gpsSpeedRef		= exifGpsTag "gpsSpeedRef" 0x000c
gpsSpeed		= exifGpsTag "gpsSpeed" 0x000d
gpsTrackRef		= exifGpsTag "gpsTrackRef" 0x000e
gpsTrack		= exifGpsTag "gpsTrack" 0x000f
gpsImgDirectionRef	= exifGpsTag "gpsImgDirectionRef" 0x0010
gpsImgDirection		= exifGpsTag "gpsImgDirection" 0x0011
gpsMapDatum		= exifGpsTag "gpsMapDatum" 0x0012
gpsDestLatitudeRef	= exifGpsTag "gpsDestLatitudeRef" 0x0013
gpsDestLatitude		= exifGpsTag "gpsDestLatitude" 0x0014
gpsDestLongitudeRef	= exifGpsTag "gpsDestLongitudeRef" 0x0015
gpsDestLongitude	= exifGpsTag "gpsDestLongitude" 0x0016
gpsDestBearingRef	= exifGpsTag "gpsDestBearingRef" 0x0017
gpsDestBearing		= exifGpsTag "gpsDestBearing" 0x0018
gpsDestDistanceRef	= exifGpsTag "gpsDestDistanceRef" 0x0019
gpsDestDistance		= exifGpsTag "gpsDestDistance" 0x001a
gpsProcessingMethod	= exifGpsTag "gpsProcessingMethod" 0x001b
gpsAreaInformation	= exifGpsTag "gpsAreaInformation" 0x001c
gpsDateStamp		= exifGpsTag "gpsDateStamp" 0x001d
gpsDifferential		= exifGpsTag "gpsDifferential" 0x001e

allExifTags :: [ExifTag]
allExifTags = [exposureTime, fnumber, exposureProgram, isoSpeedRatings,
	exifVersion, dateTimeOriginal, dateTimeDigitized, componentConfiguration,
	compressedBitsPerPixel, shutterSpeedValue, apertureValue, brightnessValue,
	exposureBiasValue, maxApertureValue, subjectDistance, meteringMode,
	lightSource, flash, focalLength, makerNote, userComment,
	exifImageWidth, exifImageHeight, relatedSoundFile, focalPlaneXResolution,
	focalPlaneYResolution, focalPlaneResolutionUnit, sensingMethod, fileSource,
	sceneType, orientation, make, model, software, copyright,
	spectralSensitivity, oecf, subjectArea, subSecTime, subSecTimeOriginal,
	subSecTimeDigitized, flashPixVersion, colorSpace, flashEnergy,
	spatialFrequencyResponse, subjectLocation, exposureIndex, cfaPattern,
	customRendered, exposureMode, whiteBalance, digitalZoomRatio,
	focalLengthIn35mmFilm, sceneCaptureType, gainControl, contrast,
	saturation, sharpness, deviceSettingDescription, subjectDistanceRange,
	imageUniqueId, exifInteroperabilityOffset, imageDescription,
	xResolution, yResolution, resolutionUnit, dateTime, whitePoint,
	primaryChromaticities, yCbCrPositioning, yCbCrCoefficients, referenceBlackWhite,
	exifIfdOffset, printImageMatching, gpsTagOffset, artist,
	gpsVersionID, gpsLatitudeRef, gpsLatitude, gpsLongitudeRef, gpsLongitude,
	gpsAltitudeRef, gpsAltitude, gpsTimeStamp, gpsSatellites, gpsStatus,
	gpsMeasureMode, gpsDop, gpsSpeedRef, gpsSpeed, gpsTrackRef, gpsTrack,
	gpsImgDirectionRef, gpsImgDirection, gpsMapDatum, gpsDestLatitudeRef,
	gpsDestLatitude, gpsDestLongitudeRef, gpsDestLongitude, gpsDestBearingRef,
	gpsDestBearing, gpsDestDistanceRef, gpsDestDistance, gpsProcessingMethod,
	gpsAreaInformation, gpsDateStamp, gpsDifferential]

getExifTag :: TagLocation -> Word16 -> ExifTag
getExifTag l v = fromMaybe (ExifTag l Nothing v) $ find (isSameTag l v) allExifTags
	where isSameTag l1 v1 (ExifTag l2 _ v2) = l1 == l2 && v1 == v2

data ValueHandler = ValueHandler
	{
		dataTypeId :: Word16,
		dataLength :: Int,
		readSingle :: ByteAlign -> Get ExifValue,
		readMany :: ByteAlign -> Int -> Get ExifValue
	}

readNumberList :: Integral a => (ByteAlign -> Get a) -> ByteAlign -> Int -> Get ExifValue
readNumberList decoder byteAlign components = liftM (ExifNumberList . fmap fromIntegral)
			$ count components (decoder byteAlign)

unsignedByteValueHandler = ValueHandler
	{
		dataTypeId = 1,
		dataLength = 1,
		readSingle = \_ -> liftM (ExifNumber . fromIntegral) getWord8,
		readMany = readNumberList $ const getWord8
	}

asciiStringValueHandler = ValueHandler
	{
		dataTypeId = 2,
		dataLength = 1,
		readSingle = \ba -> readMany asciiStringValueHandler ba 1,
		readMany = \_ components -> liftM (ExifText . Char8.unpack) (getByteString (components-1))
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
	numerator <- liftM fromIntegral $ getWord32 byteAlign
	denominator <- liftM fromIntegral $ getWord32 byteAlign
	return $ c numerator denominator

unsignedRationalValueHandler = ValueHandler
	{
		dataTypeId = 5,
		dataLength = 8,
		readSingle = readRationalContents ExifRational,
		readMany = \byteAlign components -> liftM ExifRationalList $ count components (readRationalContents (,) byteAlign)
	}

signedByteValueHandler = ValueHandler
	{
		dataTypeId = 6,
		dataLength = 1,
		readSingle = \_ -> liftM (ExifNumber . signedInt8ToInt) getWord8,
		readMany = readNumberList (liftM signedInt8ToInt . const getWord8)
	}

undefinedValueHandler = ValueHandler
	{
		dataTypeId = 7,
		dataLength = 1,
		readSingle = \ba -> readMany undefinedValueHandler ba 1,
		readMany = \_ components -> liftM ExifUndefined (getByteString (components-1))
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
	numerator <- liftM signedInt32ToInt $ getWord32 byteAlign
	denominator <- liftM signedInt32ToInt $ getWord32 byteAlign
	return $ c numerator denominator

signedRationalValueHandler = ValueHandler
	{
		dataTypeId = 10,
		dataLength = 8,
		readSingle = readSignedRationalContents ExifRational,
		readMany = \byteAlign components -> liftM ExifRationalList $ count components (readSignedRationalContents (,) byteAlign)
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
	curPos <- liftM fromIntegral bytesRead
	skip $ contentsInt + tiffHeaderStart - curPos
	bytestring <- getLazyByteString (fromIntegral $ entryNoComponents entry * dataLength handler)
	return $ parseInline byteAlign handler entry bytestring

signedInt32ToInt :: Word32 -> Int
signedInt32ToInt w = fromIntegral (fromIntegral w :: Int32)

signedInt16ToInt :: Word16 -> Int
signedInt16ToInt w = fromIntegral (fromIntegral w :: Int16)

signedInt8ToInt :: Word8 -> Int
signedInt8ToInt w = fromIntegral (fromIntegral w :: Int8)

-- i had this as runGetM and reusing in parseExif,
-- sadly fail is not implemented for Either.
-- will do for now.
runMaybeGet :: Get a -> B.ByteString -> Maybe a
runMaybeGet get bs = case runGetOrFail get bs of
	Left _ -> Nothing
	Right (_,_,x) -> Just x

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
	return $ LocalTime (fromGregorian year month day) (TimeOfDay hour minute second)
	where
		readDigit x = liftM read $ count x getDigit

count :: Int -> Get a -> Get [a]
count n p | n <= 0 = return []
        | otherwise = replicateM n p
	

-- | Extract the date and time when the picture was taken
-- from the EXIF information.
getDateTimeOriginal :: Map ExifTag ExifValue -> Maybe LocalTime
getDateTimeOriginal exifData = Map.lookup dateTimeOriginal exifData >>= readExifDateTime . show 

getCharWhere :: (Char->Bool) -> Get Char
getCharWhere wher = do
	char <- liftM (chr . fromIntegral) getWord8
	if wher char
		then return char
		else fail "no parse"

getDigit :: Get Char
getDigit = getCharWhere isDigit

getCharValue :: Char -> Get Char
getCharValue char = getCharWhere (==char)

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

-- | Extract the GPS latitude and longitude where the picture was taken
-- (if it is present in the EXIF)
getGpsLatitudeLongitude :: Map ExifTag ExifValue -> Maybe (Double, Double)
getGpsLatitudeLongitude exifData = do
	(ExifText latRef) <- Map.lookup gpsLatitudeRef exifData
	latDec <- Map.lookup gpsLatitude exifData >>= gpsDecodeToDecimalDegrees 
	let signedLatDec = case latRef of
		"S" -> -latDec
		_ -> latDec
	(ExifText longRef) <- Map.lookup gpsLongitudeRef exifData
	longDec <- Map.lookup gpsLongitude exifData >>= gpsDecodeToDecimalDegrees 
	let signedLongDec = case longRef of
		"W" -> -longDec
		_ -> longDec
	return (signedLatDec, signedLongDec)

gpsDecodeToDecimalDegrees :: ExifValue -> Maybe Double
gpsDecodeToDecimalDegrees (ExifRationalList intPairs) = case fmap intPairToFloating intPairs of
			(degrees:minutes:seconds:[]) -> Just $ degrees + minutes / 60 + seconds / 3600
			_ -> Nothing
	where
		intPairToFloating (n, d) = fromIntegral n / fromIntegral d
gpsDecodeToDecimalDegrees _ = Nothing

-- | Format the exif value as floating-point if it makes sense,
-- otherwise use the default 'show' implementation.
-- The first parameter lets you specify how many digits after
-- the comma to format in the result string.
-- The special behaviour applies only for 'ExifRational' and 'ExifRationalList'.
formatAsFloatingPoint :: Int -> ExifValue -> String
formatAsFloatingPoint n (ExifRational num den) = formatNumDenAsString n num den
formatAsFloatingPoint n (ExifRationalList values) = intercalate ", " $ foldl' step [] values
	where step soFar (num,den) = soFar ++ [formatNumDenAsString n num den]
formatAsFloatingPoint _ v = show v

formatNumDenAsString :: Int -> Int -> Int -> String
formatNumDenAsString n num den = printf formatString (fromIntegral num / fromIntegral den :: Double)
	where formatString = "%." ++ show n ++ "f"

-- $intro
--
-- EXIF parsing from JPEG files.
-- EXIF tags are enumerated as ExifTag values, check 'exposureTime' for instance.
-- If you use the predefined ExifTag values, you don't care about details
-- of the ExifTag type, however you should check out the 'ExifValue' type.
--
-- You start from a JPEG file, you can parse its exif tags as a 'Map' of
-- 'ExifTag' to 'ExifValue' using 'parseExif' or 'parseFileExif'.
-- You can enumerate the map or 'lookup' the tags that interest you.
--
-- There are also a couple of higher-level helpers like 'getOrientation',
-- 'getDateTimeOriginal'.
