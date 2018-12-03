{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Graphics.ExifTags where

import Data.Word
import Text.Printf (printf)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Binary.Get
import Data.Maybe (fromMaybe)

import Graphics.Types
import Graphics.PrettyPrinters
import Graphics.Helpers

exifSubIfdTag :: String -> Word16 -> (ExifValue -> Text)-> ExifTag
exifSubIfdTag d = ExifTag ExifSubIFD (Just d)

exifIfd0Tag :: String -> Word16 -> (ExifValue -> Text) -> ExifTag
exifIfd0Tag d = ExifTag IFD0 (Just d)

exifGpsTag :: String -> Word16 -> (ExifValue -> Text) -> ExifTag
exifGpsTag d = ExifTag GpsSubIFD (Just d)

withFormat :: String -> ExifValue -> Text
withFormat fmt = T.pack . printf fmt . show

asFpWithFormat :: String -> ExifValue -> Text
asFpWithFormat fmt = T.pack . printf fmt . formatAsFloatingPoint 2

showT :: Show a => a -> Text
showT = T.pack . show

exposureTime            = exifSubIfdTag "exposureTime" 0x829a $ ppExposureTime
fnumber                 = exifSubIfdTag "fnumber" 0x829d ppAperture
exposureProgram         = exifSubIfdTag "exposureProgram" 0x8822 ppExposureProgram
spectralSensitivity     = exifSubIfdTag "spectralSensitivity" 0x8824 showT
isoSpeedRatings         = exifSubIfdTag "isoSpeedRatings" 0x8827 showT
oecf                    = exifSubIfdTag "OECF" 0x8828 showT
exifVersion             = exifSubIfdTag "exifVersion" 0x9000 ppExifVersion
dateTimeOriginal        = exifSubIfdTag "dateTimeOriginal" 0x9003 showT
dateTimeDigitized       = exifSubIfdTag "dateTimeDigitized" 0x9004 showT
componentConfiguration  = exifSubIfdTag "componentConfiguration" 0x9101 ppComponentConfiguration
compressedBitsPerPixel  = exifSubIfdTag "compressedBitsPerPixel" 0x9102 (T.pack . formatAsFloatingPoint 2)
shutterSpeedValue       = exifSubIfdTag "shutterSpeedValue" 0x9201 $ ppExposureTime
apertureValue           = exifSubIfdTag "apertureValue" 0x9202 ppApexAperture
brightnessValue         = exifSubIfdTag "brightnessValue" 0x9203 $ asFpWithFormat "%s EV"
exposureBiasValue       = exifSubIfdTag "exposureBiasValue" 0x9204 $ asFpWithFormat "%s EV"
maxApertureValue        = exifSubIfdTag "maxApertureValue" 0x9205 ppApexAperture
subjectDistance         = exifSubIfdTag "subjectDistance" 0x9206 showT
meteringMode            = exifSubIfdTag "meteringMode" 0x9207 ppMeteringMode
lightSource             = exifSubIfdTag "lightSource" 0x9208 ppLightSource
flash                   = exifSubIfdTag "flash" 0x9209 ppFlash
focalLength             = exifSubIfdTag "focalLength" 0x920a $ asFpWithFormat "%s mm"
subjectArea             = exifSubIfdTag "subjectArea" 0x9214 showT
makerNote               = exifSubIfdTag "makerNote" 0x927c ppUndef
userComment             = exifSubIfdTag "userComment" 0x9286 ppUserComment
subSecTime              = exifSubIfdTag "subSecTime" 0x9290 showT
subSecTimeOriginal      = exifSubIfdTag "subSecTimeOriginal" 0x9291 showT
subSecTimeDigitized     = exifSubIfdTag "subSecTimeDigitized" 0x9292 showT
flashPixVersion         = exifSubIfdTag "flashPixVersion" 0xa000 ppFlashPixVersion
colorSpace              = exifSubIfdTag "colorSpace" 0xa001 ppColorSpace
exifImageWidth          = exifSubIfdTag "exifImageWidth" 0xa002 showT
exifImageHeight         = exifSubIfdTag "exifImageHeight" 0xa003 showT
relatedSoundFile        = exifSubIfdTag "relatedSoundFile" 0xa004 showT
flashEnergy             = exifSubIfdTag "flashEnergy" 0xa20b showT
spatialFrequencyResponse= exifSubIfdTag "spatialFrequencyResponse" 0xa20c showT
focalPlaneXResolution   = exifSubIfdTag "focalPlaneXResolution" 0xa20e showT
focalPlaneYResolution   = exifSubIfdTag "focalPlaneYResolution" 0xa20f showT
focalPlaneResolutionUnit= exifSubIfdTag "focalPlaneResolutionUnit" 0xa210 ppFocalPlaneResolutionUnit
subjectLocation         = exifSubIfdTag "subjectLocation" 0xa214 showT
exposureIndex           = exifSubIfdTag "exposureIndex" 0xa215 showT
sensingMethod           = exifSubIfdTag "sensingMethod" 0xa217 ppSensingMethod
fileSource              = exifSubIfdTag "fileSource" 0xa300 ppFileSource
sceneType               = exifSubIfdTag "sceneType" 0xa301 ppSceneType
cfaPattern              = exifSubIfdTag "cfaPattern" 0xa302 showT
customRendered          = exifSubIfdTag "customRendered" 0xa401 ppCustomRendered
exposureMode            = exifSubIfdTag "exposureMode" 0xa402 ppExposureMode
whiteBalance            = exifSubIfdTag "whiteBalance" 0xa403 ppWhiteBalance
digitalZoomRatio        = exifSubIfdTag "digitalZoomRatio" 0xa404 showT
focalLengthIn35mmFilm   = exifSubIfdTag "focalLengthIn35mmFilm" 0xa405 $ asFpWithFormat "%s mm"
sceneCaptureType        = exifSubIfdTag "sceneCaptureType" 0xa406 ppSceneCaptureType
gainControl             = exifSubIfdTag "gainControl" 0xa407 ppGainControl
contrast                = exifSubIfdTag "contrast" 0xa408 ppContrastSharpness
saturation              = exifSubIfdTag "saturation" 0xa409 ppSaturation
sharpness               = exifSubIfdTag "sharpness" 0xa40a ppContrastSharpness
deviceSettingDescription= exifSubIfdTag "deviceSettingDescription" 0xa40b showT
subjectDistanceRange    = exifSubIfdTag "subjectDistanceRange" 0xa40c ppSubjectDistanceRange
imageUniqueId           = exifSubIfdTag "imageUniqueId" 0xa420 showT
exifInteroperabilityOffset=exifSubIfdTag "exifInteroperabilityOffset" 0xa005 showT

imageDescription        = exifIfd0Tag "imageDescription" 0x010e showT
make                    = exifIfd0Tag "make" 0x010f showT
model                   = exifIfd0Tag "model" 0x0110 showT
orientation             = exifIfd0Tag "orientation" 0x0112 ppOrientation
xResolution             = exifIfd0Tag "xResolution" 0x011a showT
yResolution             = exifIfd0Tag "xResolution" 0x011b showT
resolutionUnit          = exifIfd0Tag "resolutionUnit" 0x0128 ppResolutionUnit
software                = exifIfd0Tag "software" 0x0131 showT
dateTime                = exifIfd0Tag "dateTime" 0x0132 showT
artist                  = exifIfd0Tag "artist" 0x013b showT
whitePoint              = exifIfd0Tag "whitePoint" 0x013e showT
primaryChromaticities   = exifIfd0Tag "primaryChromaticities" 0x013f showT
yCbCrCoefficients       = exifIfd0Tag "yCbCrCoefficients" 0x0211 showT
yCbCrPositioning        = exifIfd0Tag "yCbCrPositioning" 0x0213 ppYCbCrPositioning
referenceBlackWhite     = exifIfd0Tag "referenceBlackWhite" 0x0214 showT
copyright               = exifIfd0Tag "copyright" 0x8298 showT
printImageMatching      = exifIfd0Tag "printImageMatching" 0xc4a5 ppUndef

gpsVersionID            = exifGpsTag "gpsVersionID" 0x0000 showT
gpsLatitudeRef          = exifGpsTag "gpsLatitudeRef" 0x0001 ppGpsLatitudeRef
gpsLatitude             = exifGpsTag "gpsLatitude" 0x0002 ppGpsLongLat
gpsLongitudeRef         = exifGpsTag "gpsLongitudeRef" 0x0003 ppGpsLongitudeRef
gpsLongitude            = exifGpsTag "gpsLongitude" 0x0004 ppGpsLongLat
gpsAltitudeRef          = exifGpsTag "gpsAltitudeRef" 0x0005 ppGpsAltitudeRef
gpsAltitude             = exifGpsTag "gpsAltitude" 0x0006 (T.pack . formatAsFloatingPoint 4)
gpsTimeStamp            = exifGpsTag "gpsTimeStamp" 0x0007 ppGpsTimeStamp
gpsSatellites           = exifGpsTag "gpsSatellites" 0x0008 showT
gpsStatus               = exifGpsTag "gpsStatus" 0x0009 showT
gpsMeasureMode          = exifGpsTag "gpsMeasureMode" 0x000a showT
gpsDop                  = exifGpsTag "gpsDop" 0x000b showT
gpsSpeedRef             = exifGpsTag "gpsSpeedRef" 0x000c showT
gpsSpeed                = exifGpsTag "gpsSpeed" 0x000d showT
gpsTrackRef             = exifGpsTag "gpsTrackRef" 0x000e showT
gpsTrack                = exifGpsTag "gpsTrack" 0x000f showT
gpsImgDirectionRef      = exifGpsTag "gpsImgDirectionRef" 0x0010 showT
gpsImgDirection         = exifGpsTag "gpsImgDirection" 0x0011 showT
gpsMapDatum             = exifGpsTag "gpsMapDatum" 0x0012 showT
gpsDestLatitudeRef      = exifGpsTag "gpsDestLatitudeRef" 0x0013 showT
gpsDestLatitude         = exifGpsTag "gpsDestLatitude" 0x0014 showT
gpsDestLongitudeRef     = exifGpsTag "gpsDestLongitudeRef" 0x0015 showT
gpsDestLongitude        = exifGpsTag "gpsDestLongitude" 0x0016 showT
gpsDestBearingRef       = exifGpsTag "gpsDestBearingRef" 0x0017 showT
gpsDestBearing          = exifGpsTag "gpsDestBearing" 0x0018 showT
gpsDestDistanceRef      = exifGpsTag "gpsDestDistanceRef" 0x0019 showT
gpsDestDistance         = exifGpsTag "gpsDestDistance" 0x001a showT
gpsProcessingMethod     = exifGpsTag "gpsProcessingMethod" 0x001b showT
gpsAreaInformation      = exifGpsTag "gpsAreaInformation" 0x001c showT
gpsDateStamp            = exifGpsTag "gpsDateStamp" 0x001d ppGpsDateStamp
gpsDifferential         = exifGpsTag "gpsDifferential" 0x001e showT

allExifTags :: [ExifTag]
allExifTags = [exposureTime, fnumber, exposureProgram, isoSpeedRatings,
    exifVersion, dateTimeOriginal, dateTimeDigitized, componentConfiguration,
    compressedBitsPerPixel, shutterSpeedValue, apertureValue, brightnessValue,
    exposureBiasValue, maxApertureValue, subjectDistance, meteringMode,
    lightSource, flash, focalLength, makerNote, userComment,
    exifImageWidth, exifImageHeight, relatedSoundFile, focalPlaneXResolution,
    focalPlaneYResolution, focalPlaneResolutionUnit, sensingMethod, fileSource,
    sceneType, orientation, make, model, software, copyright,
    spectralSensitivity, oecf, subjectArea, subSecTime, subSecTimeOriginal,
    subSecTimeDigitized, flashPixVersion, colorSpace, flashEnergy,
    spatialFrequencyResponse, subjectLocation, exposureIndex, cfaPattern,
    customRendered, exposureMode, whiteBalance, digitalZoomRatio,
    focalLengthIn35mmFilm, sceneCaptureType, gainControl, contrast,
    saturation, sharpness, deviceSettingDescription, subjectDistanceRange,
    imageUniqueId, exifInteroperabilityOffset, imageDescription,
    xResolution, yResolution, resolutionUnit, dateTime, whitePoint,
    primaryChromaticities, yCbCrPositioning, yCbCrCoefficients, referenceBlackWhite,
    printImageMatching, artist,
    gpsVersionID, gpsLatitudeRef, gpsLatitude, gpsLongitudeRef, gpsLongitude,
    gpsAltitudeRef, gpsAltitude, gpsTimeStamp, gpsSatellites, gpsStatus,
    gpsMeasureMode, gpsDop, gpsSpeedRef, gpsSpeed, gpsTrackRef, gpsTrack,
    gpsImgDirectionRef, gpsImgDirection, gpsMapDatum, gpsDestLatitudeRef,
    gpsDestLatitude, gpsDestLongitudeRef, gpsDestLongitude, gpsDestBearingRef,
    gpsDestBearing, gpsDestDistanceRef, gpsDestDistance, gpsProcessingMethod,
    gpsAreaInformation, gpsDateStamp, gpsDifferential]

-- | Extract the GPS latitude and longitude where the picture was taken
-- (if it is present in the EXIF)
getGpsLatitudeLongitude :: Map ExifTag ExifValue -> Maybe (Double, Double)
getGpsLatitudeLongitude exifData = do
    (ExifText latRef) <- Map.lookup gpsLatitudeRef exifData
    latDec <- Map.lookup gpsLatitude exifData >>= gpsDecodeToDecimalDegrees
    let signedLatDec = if latRef == "S" then -latDec else latDec
    (ExifText longRef) <- Map.lookup gpsLongitudeRef exifData
    longDec <- Map.lookup gpsLongitude exifData >>= gpsDecodeToDecimalDegrees
    let signedLongDec = if longRef == "W" then -longDec else longDec
    return (signedLatDec, signedLongDec)

gpsLongLatToCoords :: ExifValue -> Maybe (Double, Double, Double)
gpsLongLatToCoords (ExifRationalList intPairs) = case fmap intPairToFloating intPairs of
    [degrees, minutes, seconds] -> Just (degrees, minutes, seconds)
    _ -> Nothing
    where
        intPairToFloating (n, d) = fromIntegral n / fromIntegral d
gpsLongLatToCoords _ = Nothing

gpsDecodeToDecimalDegrees :: ExifValue -> Maybe Double
gpsDecodeToDecimalDegrees v = do
    (degrees, minutes, seconds) <- gpsLongLatToCoords v
    return $ degrees + minutes / 60 + seconds / 3600

ppGpsLongLat :: ExifValue -> Text
ppGpsLongLat x = fromMaybe "Invalid GPS data" $ _ppGpsLongLat x

_ppGpsLongLat :: ExifValue -> Maybe Text
_ppGpsLongLat v = do
    (degrees, minutes, seconds) <- gpsLongLatToCoords v
    return $ T.pack $ printf "%.0f° %.0f' %.2f\"" degrees minutes seconds

-- | Extract the GPS date time, if present in the picture.
getGpsDateTime :: Map ExifTag ExifValue -> Maybe LocalTime
getGpsDateTime exifData = do
    gpsDate <- Map.lookup gpsDateStamp exifData >>= parseGpsDate
    gpsTime <- Map.lookup gpsTimeStamp exifData >>= parseGpsTime
    return $ LocalTime gpsDate gpsTime

parseGpsDate :: ExifValue -> Maybe Day
parseGpsDate (ExifText dateStr) = runMaybeGet getExifDate $ stringToByteString dateStr
parseGpsDate _ = Nothing

getExifDate :: Get Day
getExifDate = do
    year  <- readDigit 4
    month <- getCharValue ':' >> readDigit 2
    day   <- getCharValue ':' >> readDigit 2
    return $ fromGregorian year month day

-- | read the GPS time from the 'gpsTimeStamp' field.
parseGpsTime :: ExifValue -> Maybe TimeOfDay
parseGpsTime (ExifRationalList [(hr_n, hr_d), (min_n, min_d), (sec_n, sec_d)]) =
    Just $ TimeOfDay (hr_n `div` hr_d) (min_n `div` min_d) (fromIntegral sec_n / fromIntegral sec_d)
parseGpsTime _ = Nothing

ppGpsTimeStamp :: ExifValue -> Text
ppGpsTimeStamp exifV = maybe "invalid" (T.pack . formatTod) $ parseGpsTime exifV
    where formatTod (TimeOfDay h m s) = printf "%02d:%02d:%02.2f" h m (realToFrac s :: Float)

ppGpsDateStamp :: ExifValue -> Text
ppGpsDateStamp exifV = maybe "invalid" (T.pack . formatDay . toGregorian) $ parseGpsDate exifV
    where formatDay (year, month, day) = printf "%d-%02d-%02d" year month day

ppGpsLatitudeRef :: ExifValue -> Text
ppGpsLatitudeRef (ExifText "N") = "North"
ppGpsLatitudeRef (ExifText "S") = "South"
ppGpsLatitudeRef v@_ = T.pack $ "Invalid latitude: " ++ show v

ppGpsLongitudeRef :: ExifValue -> Text
ppGpsLongitudeRef (ExifText "E") = "East"
ppGpsLongitudeRef (ExifText "W") = "West"
ppGpsLongitudeRef v@_ = T.pack $ "Invalid longitude: " ++ show v
