module Graphics.HsExif (parseFileExif, parseExif) where

import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Control.Monad (liftM, unless)
import qualified Data.ByteString.Char8 as Char8

-- see http://www.media.mit.edu/pia/Research/deepview/exif.html

parseFileExif :: FilePath -> IO [(String, String)]
parseFileExif filename = liftM parseExif $ B.readFile filename

parseExif :: B.ByteString -> [(String, String)]
parseExif contents = case fst (runGet getExif contents) of
		Left msg -> [(msg, msg)]
		Right v -> v

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

parseExifBlock :: Int -> Get [(String,String)]
parseExifBlock blockLength = do
	header <- getByteString 4
	unless (header == Char8.pack "Exif")
		$ fail "invalid EXIF header"
	return []
