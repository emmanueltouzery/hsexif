module Graphics.Helpers where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Control.Monad (liftM, replicateM)
import Data.Char (chr, isDigit, ord)

-- i had this as runGetM and reusing in parseExif,
-- sadly fail is not implemented for Either.
-- will do for now.
runMaybeGet :: Get a -> B.ByteString -> Maybe a
runMaybeGet get bs = case runGetOrFail get bs of
	Left _ -> Nothing
	Right (_,_,x) -> Just x

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

readDigit :: Read a => Int -> Get a
readDigit x = liftM read $ count x getDigit

count :: Int -> Get a -> Get [a]
count n p | n <= 0 = return []
        | otherwise = replicateM n p

stringToByteString :: String -> B.ByteString
stringToByteString str = B.pack $ map (fromIntegral . ord) str
