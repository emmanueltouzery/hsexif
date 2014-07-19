module Graphics.Types where

import qualified Data.ByteString as BS
import Data.Word

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
