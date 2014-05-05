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

	-- * The ExifValue type
	ExifValue(..),

	-- * All known exif tags
	exposureTime,
	fnumber,
	exposureProgram,
	isoSpeedRatings,
	exifVersion,
	dateTimeOriginal,
	dateTimeDigitized,
	componentConfiguration,
	compressedBitsPerPixel,
	shutterSpeedValue,
	apertureValue,
	brightnessValue,
	exposureBiasValue,
	maxApertureValue,
	subjectDistance,
	meteringMode,
	lightSource,
	flash,
	focalLength,
	makerNote,
	userComment,
	colorSpace,
	exifImageWidth,
	exifImageHeight,
	relatedSoundFile,
	focalPlaneXResolution,
	focalPlaneYResolution,
	focalPlaneResolutionUnit,
	sensingMethod,
	fileSource,
	sceneType,
	orientation,
	make,
	model,
	software,
	copyright,

	-- * If you need to declare your own exif tags
	ExifTag(..),
	TagLocation(..),
) where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Control.Monad (liftM, unless)
import qualified Data.ByteString.Char8 as Char8
import Data.Word
import Data.Char (isDigit, ord, chr)
import Data.Int (Int32)
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Numeric (showHex)

-- | An exif value. Exif values can also be float,
-- but this library doesn't support it yet. If you
-- have JPG files containing float exif values,
-- please send it!
--
-- If you want a string describing the contents
-- of the value, simply use 'show'.
data ExifValue = ExifNumber !Int
	-- ^ An exif number. Could be short, int, signed or not.
	| ExifText !String
	-- ^ ASCII text.
	| ExifRational !Int !Int
	-- ^ A rational number (numerator, denominator).
	-- Sometimes we're used to it as rational (exposition time: 1/160),
	-- sometimes as float (exposure compensation, we rather think -0.75)
	-- 'show' will display it as 1/160.
	| ExifUnknown !Word16 !Int -- type then value
	-- ^ Unknown exif value type. Maybe float? If the JPEG file is not
	-- corrupted, please send it to me.
	deriving Eq

instance Show ExifValue where
	show (ExifNumber v) = show v
	show (ExifText v) = v
	show (ExifRational n d) = show n ++ "/" ++ show d
	show (ExifUnknown t v) = show "Unknown exif type. Type: " ++ show t ++ " value: " ++ show v

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html

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

parseExifBlock :: Get (Map ExifTag ExifValue)
parseExifBlock = do
	header <- getByteString 4
	nul <- liftM toInteger getWord16be
	unless (header == Char8.pack "Exif" && nul == 0)
		$ fail "invalid EXIF header"
	tiffHeaderStart <- liftM fromIntegral bytesRead
	byteAlign <- parseTiffHeader
	(exifSubIfdOffsetW, ifdEntries) <- parseIfd byteAlign tiffHeaderStart
	let exifSubIfdOffset = fromIntegral $ toInteger exifSubIfdOffsetW
	-- skip to the exif offset
	bytesReadNow <- liftM fromIntegral bytesRead
	skip $ (exifSubIfdOffset + tiffHeaderStart) - bytesReadNow
	exifSubEntries <- parseExifSubIfd byteAlign tiffHeaderStart
	return $ Map.fromList $ ifdEntries ++ exifSubEntries

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

parseIfd :: ByteAlign -> Int -> Get (Word32, [(ExifTag, ExifValue)])
parseIfd byteAlign tiffHeaderStart = do
	dirEntriesCount <- liftM toInteger (getWord16 byteAlign)
	ifdEntries <- mapM (\_ -> parseIfEntry byteAlign) [1..dirEntriesCount]
	let exifOffsetEntry = fromMaybe (error "Can't find the exif offset in the IFD")
		(find (\ e -> entryTag e == 0x8769) ifdEntries)
	let exifOffset = entryContents exifOffsetEntry
	entries <- mapM (decodeEntry byteAlign tiffHeaderStart IFD0) ifdEntries
	return (exifOffset, entries)

parseExifSubIfd :: ByteAlign -> Int -> Get [(ExifTag, ExifValue)]
parseExifSubIfd byteAlign tiffHeaderStart = do
	dirEntriesCount <- liftM toInteger (getWord16 byteAlign)
	ifdEntries <- mapM (\_ -> parseIfEntry byteAlign) [1..dirEntriesCount]
	mapM (decodeEntry byteAlign tiffHeaderStart ExifSubIFD) ifdEntries

data IfEntry = IfEntry
	{
		entryTag :: !Word16,
		entryFormat :: !Word16,
		entryNoComponents :: !Word32,
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
			entryNoComponents = numComponents,
			entryContents = value
		}

-- | Location of the tag in the JPG file structure.
-- Normally you don't need to fiddle with this,
-- except maybe if the library doesn't know the particular
-- exif tag you're interested in.
-- Rather check the list of supported exif tags, like
-- 'exposureTime' and so on.
data TagLocation = ExifSubIFD | IFD0
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

exposureTime		= exifSubIfdTag "exposureTime" 0x829a
fnumber			= exifSubIfdTag "fnumber" 0x829d
exposureProgram		= exifSubIfdTag "exposureProgram" 0x8822
isoSpeedRatings		= exifSubIfdTag "isoSpeedRatings" 0x8827 
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
makerNote		= exifSubIfdTag "makerNote" 0x927c 
userComment		= exifSubIfdTag "userComment" 0x9286 
colorSpace		= exifSubIfdTag "colorSpace" 0xa001 
exifImageWidth		= exifSubIfdTag "exifImageWidth" 0xa002 
exifImageHeight		= exifSubIfdTag "exifImageHeight" 0xa003 
relatedSoundFile	= exifSubIfdTag "relatedSoundFile" 0xa004 
focalPlaneXResolution	= exifSubIfdTag "focalPlaneXResolution" 0xa20e 
focalPlaneYResolution	= exifSubIfdTag "focalPlaneYResolution" 0xa20f 
focalPlaneResolutionUnit= exifSubIfdTag "focalPlaneResolutionUnit" 0xa210 
sensingMethod		= exifSubIfdTag "sensingMethod" 0xa217 
fileSource		= exifSubIfdTag "fileSource" 0xa300 
sceneType		= exifSubIfdTag "sceneType" 0xa301 
orientation		= exifIfd0Tag "orientation" 0x0112 
make			= exifIfd0Tag "make" 0x010f 
model			= exifIfd0Tag "model" 0x0110 
software		= exifIfd0Tag "software" 0x0131 
copyright		= exifIfd0Tag "copyright" 0x8298 

allExifTags :: [ExifTag]
allExifTags = [exposureTime, fnumber, exposureProgram, isoSpeedRatings,
	exifVersion, dateTimeOriginal, dateTimeDigitized, componentConfiguration,
	compressedBitsPerPixel, shutterSpeedValue, apertureValue, brightnessValue,
	exposureBiasValue, maxApertureValue, subjectDistance, meteringMode,
	lightSource, flash, focalLength, makerNote, userComment, colorSpace,
	exifImageWidth, exifImageHeight, relatedSoundFile, focalPlaneXResolution,
	focalPlaneYResolution, focalPlaneResolutionUnit, sensingMethod, fileSource,
	sceneType, orientation, make, model, software, copyright]

getExifTag :: TagLocation -> Word16 -> ExifTag
getExifTag l v = fromMaybe (ExifTag l Nothing v) $ find (isSameTag l v) allExifTags
	where isSameTag l1 v1 (ExifTag l2 _ v2) = l1 == l2 && v1 == v2

decodeEntry :: ByteAlign -> Int -> TagLocation -> IfEntry -> Get (ExifTag, ExifValue)
decodeEntry byteAlign tiffHeaderStart location entry = do
	let exifTag = getExifTag location $ entryTag entry
	let contentsInt = fromIntegral $ toInteger $ entryContents entry
	let componentsInt = fromIntegral $ toInteger $ entryNoComponents entry
	-- because I only know how to skip ahead, I hope the entries
	-- are always sorted in order of the offsets to their values...
	-- (maybe lookAhead could help here?)
	tagValue <- case entryFormat entry of
		1 -> return $ ExifNumber contentsInt -- unsigned byte
		2 -> do -- ascii string
			curPos <- liftM fromIntegral bytesRead
			skip $ contentsInt + tiffHeaderStart - curPos
			liftM (ExifText . Char8.unpack) (getByteString (componentsInt-1))
		3 -> return $ ExifNumber contentsInt -- unsigned short
		4 -> return $ ExifNumber contentsInt -- unsigned long
		5 -> do -- unsigned rational
			curPos <- liftM fromIntegral bytesRead
			skip $ contentsInt + tiffHeaderStart - curPos
			numerator <- liftM fromIntegral $ getWord32 byteAlign
			denominator <- liftM fromIntegral $ getWord32 byteAlign
			return $ ExifRational numerator denominator
		6 -> return $ ExifNumber $ signedInt32ToInt $ entryContents entry -- signed byte
		7 -> return $ ExifNumber contentsInt -- undefined
		8 -> return $ ExifNumber $ signedInt32ToInt $ entryContents entry -- signed short
		9 -> return $ ExifNumber $ signedInt32ToInt $ entryContents entry -- signed long
		10 -> do -- signed rational
			curPos <- liftM fromIntegral bytesRead
			skip $ contentsInt + tiffHeaderStart - curPos
			numerator <- liftM signedInt32ToInt (getWord32 byteAlign)
			denominator <- liftM signedInt32ToInt (getWord32 byteAlign)
			return $ ExifRational numerator denominator
		-- TODO decode float values, 11 single float and 12 double float but I'd like tests
		_ -> return $ ExifUnknown (entryFormat entry) contentsInt
	return (exifTag, tagValue)

signedInt32ToInt :: Word32 -> Int
signedInt32ToInt w = fromIntegral (fromIntegral w :: Int32)

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
        | otherwise = sequence (replicate n p)
	

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
