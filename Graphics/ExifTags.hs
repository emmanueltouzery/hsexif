{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Graphics.ExifTags where

import Data.Word
import Text.Printf (printf)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

import Graphics.Types
import Graphics.PrettyPrinters

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

exposureTime		= exifSubIfdTag "exposureTime" 0x829a $ withFormat "%s sec."
fnumber			= exifSubIfdTag "fnumber" 0x829d $ withFormat "f/%s"
exposureProgram		= exifSubIfdTag "exposureProgram" 0x8822 ppExposureProgram
spectralSensitivity	= exifSubIfdTag "spectralSensitivity" 0x8824 showT
isoSpeedRatings		= exifSubIfdTag "isoSpeedRatings" 0x8827 showT
oecf			= exifSubIfdTag "OECF" 0x8828 showT
exifVersion		= exifSubIfdTag "exifVersion" 0x9000 ppExifVersion
dateTimeOriginal	= exifSubIfdTag "dateTimeOriginal" 0x9003 showT
dateTimeDigitized	= exifSubIfdTag "dateTimeDigitized" 0x9004 showT
componentConfiguration	= exifSubIfdTag "componentConfiguration" 0x9101 ppComponentConfiguration
compressedBitsPerPixel	= exifSubIfdTag "compressedBitsPerPixel" 0x9102 (T.pack . formatAsFloatingPoint 2)
shutterSpeedValue	= exifSubIfdTag "shutterSpeedValue" 0x9201 $ withFormat "%s sec."
apertureValue		= exifSubIfdTag "apertureValue" 0x9202 showT
brightnessValue		= exifSubIfdTag "brightnessValue" 0x9203 showT
exposureBiasValue	= exifSubIfdTag "exposureBiasValue" 0x9204 $ asFpWithFormat "%s EV"
maxApertureValue	= exifSubIfdTag "maxApertureValue" 0x9205 $ withFormat "f/%s"
subjectDistance		= exifSubIfdTag "subjectDistance" 0x9206 showT
meteringMode		= exifSubIfdTag "meteringMode" 0x9207 ppMeteringMode
lightSource		= exifSubIfdTag "lightSource" 0x9208 ppLightSource
flash			= exifSubIfdTag "flash" 0x9209 ppFlash
focalLength		= exifSubIfdTag "focalLength" 0x920a $ asFpWithFormat "%s mm"
subjectArea		= exifSubIfdTag "subjectArea" 0x9214 showT
makerNote		= exifSubIfdTag "makerNote" 0x927c ppUndef
userComment		= exifSubIfdTag "userComment" 0x9286 ppUserComment
subSecTime		= exifSubIfdTag "subSecTime" 0x9290 showT
subSecTimeOriginal	= exifSubIfdTag "subSecTimeOriginal" 0x9291 showT
subSecTimeDigitized	= exifSubIfdTag "subSecTimeDigitized" 0x9292 showT
flashPixVersion		= exifSubIfdTag "flashPixVersion" 0xa000 ppFlashPixVersion
colorSpace		= exifSubIfdTag "colorSpace" 0xa001 ppColorSpace
exifImageWidth		= exifSubIfdTag "exifImageWidth" 0xa002 showT
exifImageHeight		= exifSubIfdTag "exifImageHeight" 0xa003 showT
relatedSoundFile	= exifSubIfdTag "relatedSoundFile" 0xa004 showT
flashEnergy		= exifSubIfdTag "flashEnergy" 0xa20b showT
spatialFrequencyResponse= exifSubIfdTag "spatialFrequencyResponse" 0xa20c showT
focalPlaneXResolution	= exifSubIfdTag "focalPlaneXResolution" 0xa20e showT
focalPlaneYResolution	= exifSubIfdTag "focalPlaneYResolution" 0xa20f showT
focalPlaneResolutionUnit= exifSubIfdTag "focalPlaneResolutionUnit" 0xa210 ppFocalPlaneResolutionUnit
subjectLocation		= exifSubIfdTag "subjectLocation" 0xa214 showT
exposureIndex		= exifSubIfdTag "exposureIndex" 0xa215 showT
sensingMethod		= exifSubIfdTag "sensingMethod" 0xa217 ppSensingMethod
fileSource		= exifSubIfdTag "fileSource" 0xa300 showT
sceneType		= exifSubIfdTag "sceneType" 0xa301 ppSceneType
cfaPattern		= exifSubIfdTag "cfaPattern" 0xa302 showT
customRendered		= exifSubIfdTag "customRendered" 0xa401 ppCustomRendered
exposureMode		= exifSubIfdTag "exposureMode" 0xa402 ppExposureMode
whiteBalance		= exifSubIfdTag "whiteBalance" 0xa403 ppWhiteBalance
digitalZoomRatio	= exifSubIfdTag "digitalZoomRatio" 0xa404 showT
focalLengthIn35mmFilm	= exifSubIfdTag "focalLengthIn35mmFilm" 0xa405 $ asFpWithFormat "%s mm"
sceneCaptureType	= exifSubIfdTag "sceneCaptureType" 0xa406 ppSceneCaptureType
gainControl		= exifSubIfdTag "gainControl" 0xa407 ppGainControl
contrast		= exifSubIfdTag "contrast" 0xa408 ppContrastSharpness
saturation		= exifSubIfdTag "saturation" 0xa409 ppSaturation
sharpness		= exifSubIfdTag "sharpness" 0xa40a ppContrastSharpness
deviceSettingDescription= exifSubIfdTag "deviceSettingDescription" 0xa40b showT
subjectDistanceRange	= exifSubIfdTag "subjectDistanceRange" 0xa40c ppSubjectDistanceRange
imageUniqueId		= exifSubIfdTag "imageUniqueId" 0xa420 showT
exifInteroperabilityOffset=exifSubIfdTag "exifInteroperabilityOffset" 0xa005 showT

imageDescription	= exifIfd0Tag "imageDescription" 0x010e showT
make			= exifIfd0Tag "make" 0x010f showT
model			= exifIfd0Tag "model" 0x0110 showT
orientation		= exifIfd0Tag "orientation" 0x0112 ppOrientation
xResolution		= exifIfd0Tag "xResolution" 0x011a showT
yResolution		= exifIfd0Tag "xResolution" 0x011b showT
resolutionUnit		= exifIfd0Tag "resolutionUnit" 0x0128 ppResolutionUnit
software		= exifIfd0Tag "software" 0x0131 showT
dateTime		= exifIfd0Tag "dateTime" 0x0132 showT
artist			= exifIfd0Tag "artist" 0x013b showT
whitePoint		= exifIfd0Tag "whitePoint" 0x013e showT
primaryChromaticities	= exifIfd0Tag "primaryChromaticities" 0x013f showT
yCbCrCoefficients	= exifIfd0Tag "yCbCrCoefficients" 0x0211 showT
yCbCrPositioning	= exifIfd0Tag "yCbCrPositioning" 0x0213 ppYCbCrPositioning
referenceBlackWhite	= exifIfd0Tag "referenceBlackWhite" 0x0214 showT
copyright		= exifIfd0Tag "copyright" 0x8298 showT
exifIfdOffset		= exifIfd0Tag "exifIfdOffset" 0x8769 showT
gpsTagOffset		= exifIfd0Tag "gpsTagOffset" 0x8825 showT
printImageMatching	= exifIfd0Tag "printImageMatching" 0xc4a5 ppUndef

gpsVersionID		= exifGpsTag "gpsVersionID" 0x0000 showT
gpsLatitudeRef		= exifGpsTag "gpsLatitudeRef" 0x0001 showT
gpsLatitude		= exifGpsTag "gpsLatitude" 0x0002 ppGpsLongLat
gpsLongitudeRef		= exifGpsTag "gpsLongitudeRef" 0x0003 showT
gpsLongitude		= exifGpsTag "gpsLongitude" 0x0004 ppGpsLongLat
gpsAltitudeRef		= exifGpsTag "gpsAltitudeRef" 0x0005 ppGpsAltitudeRef
gpsAltitude		= exifGpsTag "gpsAltitude" 0x0006 showT
gpsTimeStamp		= exifGpsTag "gpsTimeStamp" 0x0007 showT
gpsSatellites		= exifGpsTag "gpsSatellites" 0x0008 showT
gpsStatus		= exifGpsTag "gpsStatus" 0x0009 showT
gpsMeasureMode		= exifGpsTag "gpsMeasureMode" 0x000a showT
gpsDop			= exifGpsTag "gpsDop" 0x000b showT
gpsSpeedRef		= exifGpsTag "gpsSpeedRef" 0x000c showT
gpsSpeed		= exifGpsTag "gpsSpeed" 0x000d showT
gpsTrackRef		= exifGpsTag "gpsTrackRef" 0x000e showT
gpsTrack		= exifGpsTag "gpsTrack" 0x000f showT
gpsImgDirectionRef	= exifGpsTag "gpsImgDirectionRef" 0x0010 showT
gpsImgDirection		= exifGpsTag "gpsImgDirection" 0x0011 showT
gpsMapDatum		= exifGpsTag "gpsMapDatum" 0x0012 showT
gpsDestLatitudeRef	= exifGpsTag "gpsDestLatitudeRef" 0x0013 showT
gpsDestLatitude		= exifGpsTag "gpsDestLatitude" 0x0014 showT
gpsDestLongitudeRef	= exifGpsTag "gpsDestLongitudeRef" 0x0015 showT
gpsDestLongitude	= exifGpsTag "gpsDestLongitude" 0x0016 showT
gpsDestBearingRef	= exifGpsTag "gpsDestBearingRef" 0x0017 showT
gpsDestBearing		= exifGpsTag "gpsDestBearing" 0x0018 showT
gpsDestDistanceRef	= exifGpsTag "gpsDestDistanceRef" 0x0019 showT
gpsDestDistance		= exifGpsTag "gpsDestDistance" 0x001a showT
gpsProcessingMethod	= exifGpsTag "gpsProcessingMethod" 0x001b showT
gpsAreaInformation	= exifGpsTag "gpsAreaInformation" 0x001c showT
gpsDateStamp		= exifGpsTag "gpsDateStamp" 0x001d showT
gpsDifferential		= exifGpsTag "gpsDifferential" 0x001e showT

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
	exifIfdOffset, printImageMatching, gpsTagOffset, artist,
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
	let signedLatDec = case latRef of
		"S" -> -latDec
		_ -> latDec
	(ExifText longRef) <- Map.lookup gpsLongitudeRef exifData
	longDec <- Map.lookup gpsLongitude exifData >>= gpsDecodeToDecimalDegrees 
	let signedLongDec = case longRef of
		"W" -> -longDec
		_ -> longDec
	return (signedLatDec, signedLongDec)

gpsDecodeToDecimalDegrees :: ExifValue -> Maybe Double
gpsDecodeToDecimalDegrees (ExifRationalList intPairs) = case fmap intPairToFloating intPairs of
			(degrees:minutes:seconds:[]) -> Just $ degrees + minutes / 60 + seconds / 3600
			_ -> Nothing
	where
		intPairToFloating (n, d) = fromIntegral n / fromIntegral d
gpsDecodeToDecimalDegrees _ = Nothing

ppGpsLongLat :: ExifValue -> Text
ppGpsLongLat v = maybe "invalid GPS data" fmt $ gpsDecodeToDecimalDegrees v
	where fmt = T.pack . printf "%.6f"
