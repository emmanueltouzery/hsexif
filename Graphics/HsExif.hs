module Graphics.HsExif (ExifTag(..), parseFileExif, parseExif) where

import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Control.Monad (liftM, unless)
import qualified Data.ByteString.Char8 as Char8
import Data.Word
import Data.Int
import Data.List

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html

parseFileExif :: FilePath -> IO (Either String [(ExifTag, String)])
parseFileExif filename = liftM parseExif $ B.readFile filename

parseExif :: B.ByteString -> Either String [(ExifTag, String)]
parseExif contents = fst $ runGet getExif contents

getExif :: Get [(ExifTag,String)]
getExif = do
	header <- getWord16be
	unless (header == 0xffd8)
		$ fail "Not a JPEG file"
	findAndParseExifBlock

findAndParseExifBlock :: Get [(ExifTag, String)]
findAndParseExifBlock = do
	markerNumber <- getWord16be
	dataSize <- liftM (fromIntegral . toInteger) getWord16be
	case markerNumber of
		0xffe1 -> parseExifBlock dataSize
		_ -> skip (dataSize-2) >> findAndParseExifBlock

data ByteAlign = Intel | Motorola

getWord16 :: ByteAlign -> Get Word16
getWord16 Intel = getWord16le
getWord16 Motorola = getWord16be

getWord32 :: ByteAlign -> Get Word32
getWord32 Intel = getWord32le
getWord32 Motorola = getWord32be

parseExifBlock :: Int -> Get [(ExifTag,String)]
parseExifBlock blockLength = do
	header <- getByteString 4
	null <- liftM toInteger getWord16be
	unless (header == Char8.pack "Exif" && null == 0)
		$ fail "invalid EXIF header"
	tiffHeaderStart <- bytesRead
	byteAlign <- parseTiffHeader
	exifSubIfdOffset <- liftM (fromIntegral . toInteger) (parseIfd byteAlign tiffHeaderStart)
	-- skip to the exif offset
	bytesReadNow <- bytesRead
	skip $ (exifSubIfdOffset + tiffHeaderStart) - bytesReadNow
	parseExifSubIfd byteAlign tiffHeaderStart

parseTiffHeader :: Get ByteAlign
parseTiffHeader = do
	byteAlignV <- getByteString 2
	let byteAlign = case Char8.unpack byteAlignV of
		"II" -> Intel
		"MM" -> Motorola
	alignControl <- liftM toInteger (getWord16 byteAlign)
	unless (alignControl == 0x2a)
		$ fail "exif byte alignment mismatch"
	ifdOffset <- liftM (fromIntegral . toInteger) (getWord32 byteAlign)
	skip $ ifdOffset - 8
	return byteAlign

parseIfd :: ByteAlign -> Int -> Get Word32
parseIfd byteAlign tiffHeaderStart = do
	dirEntriesCount <- liftM toInteger (getWord16 byteAlign)
	ifdEntries <- mapM (\_ -> parseIfEntry byteAlign tiffHeaderStart) [1..dirEntriesCount]
	let exifOffsetEntry = case find (\e -> entryTag e == 0x8769) ifdEntries of
		Just x -> x
		Nothing -> error "Can't find the exif offset in the IFD"
	let exifOffset = entryContents exifOffsetEntry
	return exifOffset

parseExifSubIfd :: ByteAlign -> Int -> Get [(ExifTag,String)]
parseExifSubIfd byteAlign tiffHeaderStart = do
	dirEntriesCount <- liftM toInteger (getWord16 byteAlign)
	ifdEntries <- mapM (\_ -> parseIfEntry byteAlign tiffHeaderStart) [1..dirEntriesCount]
	--fail $ show ifdEntries
	decodedIfEntries <- mapM (decodeEntry byteAlign tiffHeaderStart) ifdEntries
	return decodedIfEntries

data IfEntry = IfEntry
	{
		entryTag :: Word16,
		entryFormat :: Word16,
		entryNoComponents :: Word32,
		entryContents :: Word32
	} deriving Show

parseIfEntry :: ByteAlign -> Int -> Get IfEntry
parseIfEntry byteAlign tiffHeaderStart = do
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

data ExifTag = ExposureTime
	| FNumber
	| ExposureProgram
	| ISOSpeedRatings
	| ExifVersion
	| DateTimeOriginal
	| DateTimeDigitized
	| ComponentConfiguration
	| CompressedBitsPerPixel
	| ShutterSpeedValue
	| ApertureValue
	| BrightnessValue
	| ExposureBiasValue
	| MaxApertureValue
	| SubjectDistance
	| MeteringMode
	| LightSource
	| Flash
	| FocalLength
	| MakerNote
	| UserComment
	| FlashPixVersion
	| ColorSpace
	| ExifImageWidth
	| ExifImageHeight
	| RelatedSoundFile
	| ExifInteroperabilityOffset
	| FocalPlaneXResolution
	| FocalPlaneYResolution
	| FocalPlaneResolutionUnit
	| SensingMethod
	| FileSource
	| SceneType
	| Unknown Word16
	deriving (Eq, Show)

getExifTag :: Word16 -> ExifTag
getExifTag entryTag = case entryTag of
	0x829a -> ExposureTime
	0x829d -> FNumber
	0x8822 -> ExposureProgram
	0x8827 -> ISOSpeedRatings
	0x9000 -> ExifVersion
	0x9003 -> DateTimeOriginal
	0x9004 -> DateTimeDigitized
	0x9101 -> ComponentConfiguration
	0x9102 -> CompressedBitsPerPixel
	0x9201 -> ShutterSpeedValue
	0x9202 -> ApertureValue
	0x9203 -> BrightnessValue
	0x9204 -> ExposureBiasValue
	0x9205 -> MaxApertureValue
	0x9206 -> SubjectDistance
	0x9207 -> MeteringMode
	0x9208 -> LightSource
	0x9209 -> Flash
	0x920a -> FocalLength
	0x927c -> MakerNote
	0x9286 -> UserComment
	0xa000 -> FlashPixVersion
	0xa001 -> ColorSpace
	0xa002 -> ExifImageWidth
	0xa003 -> ExifImageHeight
	0xa004 -> RelatedSoundFile
	0xa005 -> ExifInteroperabilityOffset
	0xa20e -> FocalPlaneXResolution
	0xa20f -> FocalPlaneYResolution
	0xa210 -> FocalPlaneResolutionUnit
	0xa217 -> SensingMethod
	0xa300 -> FileSource
	0xa301 -> SceneType
	_ -> Unknown entryTag

decodeEntry :: ByteAlign -> Int -> IfEntry -> Get (ExifTag, String)
decodeEntry byteAlign tiffHeaderStart entry = do
	let tagKey = getExifTag $ entryTag entry
	let contentsInt = fromIntegral $ toInteger $ entryContents entry
	let componentsInt = fromIntegral $ toInteger $ entryNoComponents entry
	-- because I only know how to skip ahead, I hope the entries
	-- are always sorted in order of the offsets to their values...
	-- (maybe lookAhead could help here?)
	tagValue <- case entryFormat entry of
		1 -> return $ show contentsInt -- unsigned byte
		2 -> do -- ascii string
			curPos <- bytesRead
			skip $ contentsInt + tiffHeaderStart - curPos
			valStr <- liftM Char8.unpack (getByteString (componentsInt-1))
			return valStr
		3 -> return $ show contentsInt -- unsigned short
		4 -> return $ show contentsInt -- unsigned long
		5 -> do -- unsigned rational
			curPos <- bytesRead
			skip $ contentsInt + tiffHeaderStart - curPos
			numerator <- getWord32 byteAlign
			denominator <- getWord32 byteAlign
			return $ show numerator ++ "/" ++ show denominator
		6 -> return $ show $ word32toint32 $ entryContents entry -- signed byte
		7 -> return $ show contentsInt -- undefined
		8 -> return $ show $ word32toint32 $ entryContents entry -- signed short
		9 -> return $ show $ word32toint32 $ entryContents entry -- signed long
		10 -> do -- signed rational
			curPos <- bytesRead
			skip $ contentsInt + tiffHeaderStart - curPos
			numerator <- liftM word32toint32 (getWord32 byteAlign)
			denominator <- liftM word32toint32 (getWord32 byteAlign)
			return $ show numerator ++ "/" ++ show denominator
		-- TODO decode float values, 11 single float and 12 double float but I'd like tests
		_ -> return $ "type: " ++ show (entryFormat entry) ++ " -> " ++ show contentsInt
	return (tagKey, tagValue)

word32toint32 :: Word32 -> Int32
word32toint32 word = fromIntegral word :: Int32
