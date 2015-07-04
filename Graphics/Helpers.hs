module Graphics.Helpers where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Control.Monad (replicateM)
import Control.Applicative ( (<$>) )
import Data.Char (chr, isDigit, ord)

-- | Suppress the 'Left' value of an 'Either'
-- From the 'errors' package.
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

runEitherGet :: Get a -> B.ByteString -> Either String a
runEitherGet get bs = case runGetOrFail get bs of
    Left (_,_,errorMsg) -> Left errorMsg
    Right (_,_,x)       -> Right x

runMaybeGet :: Get a -> B.ByteString -> Maybe a
runMaybeGet get = hush . runEitherGet get

getCharWhere :: (Char->Bool) -> Get Char
getCharWhere wher = do
    char <- chr . fromIntegral <$> getWord8
    if wher char
        then return char
        else fail "no parse"

getDigit :: Get Char
getDigit = getCharWhere isDigit

getCharValue :: Char -> Get Char
getCharValue char = getCharWhere (==char)

readDigit :: Read a => Int -> Get a
readDigit x = read <$> count x getDigit

count :: Int -> Get a -> Get [a]
count = replicateM

stringToByteString :: String -> B.ByteString
stringToByteString = B.pack . map (fromIntegral . ord)
