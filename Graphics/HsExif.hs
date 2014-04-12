module Graphics.HsExif (parseFileExif, parseExif) where

-- TODO move to lazy binary+lazy bytestrings.
-- for tests i can wrap a strict bytestring in a lazy one
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Control.Monad (liftM, unless)
import qualified Data.ByteString.Char8 as Char8
import Data.Word

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html

parseFileExif :: FilePath -> IO (Either String [(String, String)])
parseFileExif filename = liftM parseExif $ B.readFile filename

parseExif :: B.ByteString -> Either String [(String, String)]
parseExif contents = fst $ runGet getExif contents

getExif :: Get [(String,String)]
getExif = do
	header <- getWord16be
	unless (header == 0xffd8)
		$ fail "Not a JPEG file"
	findAndParseExifBlock

findAndParseExifBlock :: Get [(String, String)]
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

parseExifBlock :: Int -> Get [(String,String)]
parseExifBlock blockLength = do
	header <- getByteString 4
	null <- liftM toInteger getWord16be
	unless (header == Char8.pack "Exif" && null == 0)
		$ fail "invalid EXIF header"
	byteAlign <- parseTiffHeader
	parseIfd byteAlign
	return []

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

parseIfd :: ByteAlign -> Get ()
parseIfd byteAlign = do
	dirEntriesCount <- liftM toInteger (getWord32 byteAlign)
	return ()
