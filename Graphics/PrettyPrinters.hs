{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, CPP #-}
module Graphics.PrettyPrinters where

import Text.Printf (printf)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Control.Arrow (first)
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text (Text)
#if ICONV
import qualified Data.ByteString.Lazy as BL
import Codec.Text.IConv (convertFuzzy, EncodingName, Fuzzy(Transliterate))
#endif

import Graphics.Types (ExifValue(..), formatAsFloatingPoint, formatAsNumber)

-- ! Pretty print the undefined data
ppUndef :: ExifValue -> Text
ppUndef (ExifUndefined str) = T.pack (show $ BS.length str) `T.append` " bytes undefined data"
ppUndef _ = "undefined data"

ppResolutionUnit :: ExifValue -> Text
ppResolutionUnit = fromNumberMap [(1, "No absolute unit"),
            (2, "Inch"),
            (3, "Centimeter")]

ppYCbCrPositioning :: ExifValue -> Text
ppYCbCrPositioning = fromNumberMap [(1, "Centered"), (2, "Co-sited")]

ppAperture :: ExifValue -> Text
ppAperture = T.pack . printf "f/%s" . formatAsFloatingPoint 1

-- ! Pretty-print aperture stored as APEX value.
-- See: https://photo.stackexchange.com/questions/19143/how-can-the-aperture-value-written-in-exif-be-larger-than-the-nominal-limit-of-t
ppApexAperture :: ExifValue -> Text
ppApexAperture v = T.pack (printf "f/%.1f" fnumber)
  where
    doubleValue :: ExifValue -> Double
    doubleValue (ExifNumber n) = fromIntegral n
    doubleValue (ExifRational n d) = fromIntegral n / fromIntegral d
    doubleValue _ = 0
    apex = doubleValue v
    fnumber = 2 ** (apex / 2)

-- ! Pretty print exposure time.
-- Formats times slower than 1/4 with one decimal place and
-- faster times as fractions.
-- Examples: "4 sec.", "1/125 sec.", "0.8 sec."
ppExposureTime :: ExifValue -> Text
ppExposureTime v@(ExifRational num den)
           = let seconds = fromIntegral num / (fromIntegral den :: Double)
                 value | seconds <= 0.25 && seconds > 0 = "1/" ++ show ((round (1 / seconds)) :: Int)
                       | otherwise = formatAsNumber 1 v
             in T.append (T.pack value) " sec."
ppExposureTime v = T.pack (show v)

ppExposureProgram :: ExifValue -> Text
ppExposureProgram = fromNumberMap [(0, "Not defined"),
            (1, "Manual"),
            (2, "Normal program"),
            (3, "Aperture priority"),
            (4, "Shutter priority"),
            (5, "Creative program (biased toward depth of field)"),
            (6, "Action program (biased toward fast shutter speed)"),
            (7, "Portrait mode (for closeup photos with the background out of focus)"),
            (8, "Landscape mode (for landscape photos with the background in focus)")]

ppMeteringMode :: ExifValue -> Text
ppMeteringMode = fromNumberMap [(0, "Unknown"),
            (1, "Average"),
            (2, "Center-weighted average"),
            (3, "Spot"),
            (4, "MultiSpot"),
            (5, "Pattern"),
            (6, "Partial"),
            (255, "other")]

ppLightSource :: ExifValue -> Text
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

ppFlash :: ExifValue -> Text
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

ppColorSpace :: ExifValue -> Text
ppColorSpace = fromNumberMap [(1, "sRGB"), (65535, "Uncalibrated")]

ppCustomRendered :: ExifValue -> Text
ppCustomRendered = fromNumberMap [(0, "Normal process"), (1, "Custom process")]

ppExposureMode :: ExifValue -> Text
ppExposureMode = fromNumberMap [(0, "Auto exposure"),
            (1, "Manual exposure"),
            (2, "Auto bracket")]

ppWhiteBalance :: ExifValue -> Text
ppWhiteBalance = fromNumberMap [(0, "Auto white balance"),
            (1, "Manual white balance")]

ppSceneCaptureType :: ExifValue -> Text
ppSceneCaptureType = fromNumberMap [(0, "Standard"),
            (1, "Landscape"),
            (2, "Portrait"),
            (3, "Night scene")]

ppGainControl :: ExifValue -> Text
ppGainControl = fromNumberMap [(0, "Normal"),
            (1, "Low gain up"),
            (2, "High gain up"),
            (3, "Low gain down"),
            (4, "High gain down")]

ppContrastSharpness :: ExifValue -> Text
ppContrastSharpness = fromNumberMap [(0, "Normal"), (1, "Soft"), (2, "Hard")]

ppSaturation :: ExifValue -> Text
ppSaturation = fromNumberMap [(0, "Normal"),
            (1, "Low saturation"),
            (2, "High saturation")]

ppSensingMethod :: ExifValue -> Text
ppSensingMethod = fromNumberMap [(1, "Not defined"),
            (2, "One-chip color area sensor"),
            (3, "Two-chip color area sensor"),
            (4, "Three-chip color area sensor"),
            (5, "Color sequential area sensor"),
            (7, "Trilinear sensor"),
            (8, "Color sequential linear sensor")]

ppSubjectDistanceRange :: ExifValue -> Text
ppSubjectDistanceRange = fromNumberMap [(0, "Unknown"),
            (1, "Macro"),
            (2, "Close view"),
            (3, "Distance view")]

ppFocalPlaneResolutionUnit :: ExifValue -> Text
ppFocalPlaneResolutionUnit = fromNumberMap [(1, "No absolute unit of measurement"),
            (2, "Inch"),
            (3, "Centimeter")]

ppOrientation :: ExifValue -> Text
ppOrientation = fromNumberMap [(1, "Top-left"),
            (2, "Top-right"),
            (3, "Bottom-right"),
            (4, "Bottom-left"),
            (5, "Left-top"),
            (6, "Right-top"),
            (7, "Right-bottom"),
            (8, "Left-bottom")]

ppSceneType :: ExifValue -> Text
ppSceneType = fromNumberMap [(1, "Directly photographed")]

ppGpsAltitudeRef :: ExifValue -> Text
ppGpsAltitudeRef = fromNumberMap [(0, "Sea level"),
                (1, "Below sea level")]

componentMap :: Map Int Text
componentMap = Map.fromList $ zip [0..] ["-", "Y", "Cb", "Cr", "R", "G", "B"]

ppComponentConfiguration :: ExifValue -> Text
ppComponentConfiguration (ExifUndefined bs) = T.concat $ map formatComponent numbers
    where
        numbers = fromIntegral <$> BS.unpack bs
        formatComponent = fromMaybe "?" . flip Map.lookup componentMap
ppComponentConfiguration v@_ = unknown v

ppFlashPixVersion :: ExifValue -> Text
ppFlashPixVersion = formatVersion "FlashPix version %.1f"

ppExifVersion :: ExifValue -> Text
ppExifVersion = formatVersion "Exif version %.2f"

formatVersion :: String -> ExifValue -> Text
formatVersion fmt (ExifUndefined s) = T.pack $ printf fmt num
    where
        num :: Float = read asStr / 100.0
        asStr = T.unpack $ decodeUtf8 s
formatVersion _ v@_ = unknown v

#if ICONV

getIconvEncodingName :: Text -> EncodingName
getIconvEncodingName "JIS" = "SJIS"
getIconvEncodingName x@_ = T.unpack x -- ASCII and UNICODE work out of the box.

ppUserComment :: ExifValue -> Text
ppUserComment (ExifUndefined v) = decodeUtf8 $ BL.toStrict $ convertFuzzy Transliterate encoding "UTF8" rawText
    where
        encoding = getIconvEncodingName $ decodeUtf8 $ BS.take 8 v
        rawText = BL.fromStrict $ BS.drop 8 v
ppUserComment v@_ = unknown v

#else

ppUserComment :: ExifValue -> Text
ppUserComment (ExifUndefined v) = decodeUtf8 $ BS.drop 8 v
ppUserComment v@_ = unknown v

#endif

-- | Pretty printer for the FileSource tag
ppFileSource :: ExifValue -> Text
ppFileSource (ExifUndefined v)
    | BS.head v == 3 = "DSC"
    | otherwise      = "(unknown)"
ppFileSource v = unknown v

fromNumberMap :: [(Int, Text)] -> ExifValue -> Text
fromNumberMap m = fromMap convertedMap
    where convertedMap = fmap (first ExifNumber) m

fromMap :: [(ExifValue, Text)] -> ExifValue -> Text
fromMap m v = Map.findWithDefault (unknown v) v $ Map.fromList m

unknown :: Show a => a -> Text
unknown v = T.pack $ "Unknown value: " ++ show v
