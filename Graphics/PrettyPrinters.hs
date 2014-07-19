{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.PrettyPrinters where

import Graphics.Types (ExifValue(..))
import Data.List (foldl')
import Text.Printf (printf)
import qualified Data.Map as Map
import Data.Char (chr)
import qualified Data.ByteString as BS

ppResolutionUnit :: ExifValue -> String
ppResolutionUnit = fromNumberMap [(1, "No absolute unit"),
			(2, "Inch"),
			(3, "Centimeter")]

ppYCbCrPositioning :: ExifValue -> String
ppYCbCrPositioning = fromNumberMap [(1, "Centered"), (2, "Co-sited")]

ppExposureProgram :: ExifValue -> String
ppExposureProgram = fromNumberMap [(0, "Not defined"),
			(1, "Manual"),
			(2, "Normal program"),
			(3, "Aperture priority"),
			(4, "Shutter priority"),
			(5, "Creative program (biased toward depth of field)"),
			(6, "Action program (biased toward fast shutter speed)"),
			(7, "Portrait mode (for closeup photos with the background out of focus)"),
			(8, "Landscape mode (for landscape photos with the background in focus)")]

ppMeteringMode :: ExifValue -> String
ppMeteringMode = fromNumberMap [(0, "Unknown"),
			(1, "Average"),
			(2, "Center-weighted average"),
			(3, "Spot"),
			(4, "MultiSpot"),
			(5, "Pattern"),
			(6, "Partial"),
			(255, "other")]

ppLightSource :: ExifValue -> String
ppLightSource = fromNumberMap [(0, "Unknown"),
			(1, "Daylight"),
			(2, "Fluorescent"),
			(3, "Tungsten (incandescent light)"),
			(4, "Flash"),
			(9, "Fine weather"),
			(10, "Cloudy weather"),
			(11, "Shade"),
			(12, "Daylight fluorescent (D 5700 - 7100K)"),
			(13, "Day white fluorescent (N 4600 - 5400K)"),
			(14, "Cool white fluorescent (W 3900 - 4500K)"),
			(15, "White fluorescent (WW 3200 - 3700K)"),
			(17, "Standard light A"),
			(18, "Standard light B"),
			(19, "Standard light C"),
			(20, "D55"),
			(21, "D65"),
			(22, "D75"),
			(23, "D50"),
			(24, "ISO studio tungsten"),
			(255, "Other light source")]

ppFlash :: ExifValue -> String
ppFlash = fromNumberMap [(0, "Flash did not fire"),
			(1, "Flash fired"),
			(5, "Strobe return light not detected"),
			(7, "Strobe return light detected"),
			(9, "Flash fired, compulsory flash mode"),
			(0x0D, "Flash fired, compulsory flash mode, return light not detected"),
			(0x0F, "Flash fired, compulsory flash mode, return light detected"),
			(0x10, "Flash did not fire, compulsory flash mode"),
			(0x18, "Flash did not fire, auto mode"),
			(0x19, "Flash fired, auto mode"),
			(0x1D, "Flash fired, auto mode, return light not detected"),
			(0x1F, "Flash fired, auto mode, return light detected"),
			(0x20, "No flash function"),
			(0x41, "Flash fired, red-eye reduction mode"),
			(0x45, "Flash fired, red-eye reduction mode, return light not detected"),
			(0x47, "Flash fired, red-eye reduction mode, return light detected"),
			(0x49, "Flash fired, compulsory flash mode, red-eye reduction mode"),
			(0x4D, "Flash fired, compulsory flash mode, red-eye reduction mode, return light not detected"),
			(0x4F, "Flash fired, compulsory flash mode, red-eye reduction mode, return light detected"),
			(0x59, "Flash fired, auto mode, red-eye reduction mode"),
			(0x5D, "Flash fired, auto mode, return light not detected, red-eye reduction mode"),
			(0x5F, "Flash fired, auto mode, return light detected, red-eye reduction mode")]

ppColorSpace :: ExifValue -> String
ppColorSpace = fromNumberMap [(1, "sRGB"), (65535, "Uncalibrated")]

ppCustomRendered :: ExifValue -> String
ppCustomRendered = fromNumberMap [(0, "Normal process"), (1, "Custom process")]

ppExposureMode :: ExifValue -> String
ppExposureMode = fromNumberMap [(0, "Auto exposure"),
			(1, "Manual exposure"),
			(2, "Auto bracket")]

ppWhiteBalance :: ExifValue -> String
ppWhiteBalance = fromNumberMap [(0, "Auto white balance"),
			(1, "Manual white balance")]

ppSceneCaptureType :: ExifValue -> String
ppSceneCaptureType = fromNumberMap [(0, "Standard"),
			(1, "Landscape"),
			(2, "Portrait"),
			(3, "Night scene")]

ppGainControl :: ExifValue -> String
ppGainControl = fromNumberMap [(0, "Normal"),
			(1, "Low gain up"),
			(2, "High gain up"),
			(3, "Low gain down"),
			(4, "High gain down")]

ppContrastSharpness :: ExifValue -> String
ppContrastSharpness = fromNumberMap [(0, "Normal"), (1, "Soft"), (2, "Hard")]

ppSaturation :: ExifValue -> String
ppSaturation = fromNumberMap [(0, "Normal"),
			(1, "Low saturation"),
			(2, "High saturation")]

ppSensingMethod :: ExifValue -> String
ppSensingMethod = fromNumberMap [(1, "Not defined"),
			(2, "One-chip color area sensor"),
			(3, "Two-chip color area sensor"),
			(4, "Three-chip color area sensor"),
			(5, "Color sequential area sensor"),
			(7, "Trilinear sensor"),
			(8, "Color sequential linear sensor")]

ppSubjectDistanceRange :: ExifValue -> String
ppSubjectDistanceRange = fromNumberMap [(0, "Unknown"),
			(1, "Macro"),
			(2, "Close view"),
			(3, "Distance view")]

ppFocalPlaneResolutionUnit :: ExifValue -> String
ppFocalPlaneResolutionUnit = fromNumberMap [(1, "No absolute unit of measurement"),
			(2, "Inch"),
			(3, "Centimeter")]

ppOrientation :: ExifValue -> String
ppOrientation = fromNumberMap [(1, "Top-left"),
			(2, "Top-right"),
			(3, "Bottom-right"),
			(4, "Bottom-left"),
			(5, "Left-top"),
			(6, "Right-top"),
			(7, "Right-bottom"),
			(8, "Left-bottom")]

ppSceneType :: ExifValue -> String
ppSceneType = fromNumberMap [(1, "Directly photographed")]

ppGpsAltitudeRef :: ExifValue -> String
ppGpsAltitudeRef = fromNumberMap [(0, "Sea level"),
    			(1, "Below sea level")]

ppComponentConfiguration :: ExifValue -> String
ppComponentConfiguration (ExifUndefined bs) = foldl' addComponent [] numbers
	where
		numbers :: [Int] = fmap fromIntegral $ BS.unpack bs
		addComponent soFar c = soFar ++ formatComponent c
		formatComponent c = case c of
			0 -> "-"
			1 -> "Y"
			2 -> "Cb"
			3 -> "Cr"
			4 -> "R"
			5 -> "G"
			6 -> "B"
			_ -> "?"
ppComponentConfiguration v@_ = unknown v

ppFlashPixVersion :: ExifValue -> String
ppFlashPixVersion = formatVersion "FlashPix version %.1f"

ppExifVersion :: ExifValue -> String
ppExifVersion = formatVersion "Exif version %.2f"

formatVersion :: String -> ExifValue -> String
formatVersion fmt (ExifUndefined s) = printf fmt num
	where
		num :: Float = read asStr / 10.0
		asStr = fmap (chr . fromIntegral) $ BS.unpack s
formatVersion _ v@_ = unknown v

fromNumberMap :: [(Int, String)] -> ExifValue -> String
fromNumberMap m = fromMap convertedMap
	where convertedMap = fmap (\(n, s) -> (ExifNumber n, s)) m

fromMap :: [(ExifValue, String)] -> ExifValue -> String
fromMap m v = Map.findWithDefault (unknown v) v $ Map.fromList m

unknown :: Show a => a -> String
unknown v = "Unknown value: " ++ show v
