module Graphics.Types where

import qualified Data.ByteString as BS
import Data.Word
import Numeric (showHex)
import Data.Function (on)
import Data.List
import Text.Printf (printf)

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
	deriving (Eq, Ord)

instance Show ExifValue where
	show (ExifNumber v) = show v
	show (ExifText v) = v
	show (ExifRational n d) = show n ++ "/" ++ show d
	show (ExifUnknown t c v) = show "Unknown exif type. Type: " ++ show t 
		++ " count: " ++ show c ++ " value: " ++ show v
	show (ExifNumberList l) = show l
	show (ExifRationalList l) = show l
	show (ExifUndefined bs) = show bs

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
		tagKey :: Word16,
		-- ^ Exif tag key, the number uniquely identifying this tag.
		prettyPrinter :: ExifValue -> String
		-- ^ A function that'll display nicely an exif value for that exif tag.
		-- For instance for the 'flash' ExifTag, it'll say whether the flash was
		-- fired or not, if there was return light and so on.
	}

instance Show ExifTag where
	show (ExifTag _ (Just d) _ _) = d
	show (ExifTag l _ v _) = "Unknown tag, location: " ++ show l
		++ ", value: 0x" ++ showHex v ""

instance Eq ExifTag where
	t1 == t2 = tagKey t1 == tagKey t2 && tagLocation t1 == tagLocation t2

instance Ord ExifTag where
	compare t1 t2 = if locCmp /= EQ then locCmp else tagCmp
		where
			locCmp = (compare `on` tagLocation) t1 t2 
			tagCmp = (compare `on` tagKey) t1 t2

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
