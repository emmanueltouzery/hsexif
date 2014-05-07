{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.HUnit

import Graphics.HsExif
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Char (chr)

main :: IO ()
main = do
	imageContents <- B.readFile "tests/test.jpg"
	noExif <- B.readFile "tests/noexif.jpg"
	png <- B.readFile "tests/test.png"
	gps <- B.readFile "tests/gps.jpg"
	let parseExifCorrect = (\(Right x) -> x) . parseExif
	let exifData = parseExifCorrect imageContents
	let gpsExifData = parseExifCorrect gps
	hspec $ do
		describe "not a JPG" $ testNotAJpeg png
		describe "no EXIF" $ testNoExif noExif
		describe "basic parsing" $ testBasic imageContents
		describe "extract picture date" $ testDate exifData
		describe "image orientation" $ testOrientation exifData
		describe "read exif date time" $ testReadExifDateTime
		describe "read GPS lat long" $ testReadGpsLatLong gpsExifData

testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
	assertEqual' (Left "Not a JPEG file") (parseExif imageContents)

testNoExif :: B.ByteString -> Spec
testNoExif imageContents = it "returns empty list if no EXIF" $
	assertEqual' (Left "No EXIF in JPEG") (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $ do
	assertEqual' 
		(Map.fromList
		[
			(exposureTime, ExifRational 1 160),
			(fnumber, ExifRational 0 10),
			(exposureProgram, ExifNumber 2),
			(isoSpeedRatings, ExifNumber 1600),
			(exifVersion, ExifUndefined "023"),
			(dateTimeOriginal, ExifText "2013:10:02 20:33:33"),
			(dateTimeDigitized, ExifText "2013:10:02 20:33:33"),
			(componentConfiguration, ExifUndefined "\SOH\STX\ETX"),
			(compressedBitsPerPixel, ExifRational 2 1),
			(brightnessValue, ExifRational (-4226) 2560),
			(exposureBiasValue, ExifRational 0 10),
			(maxApertureValue, ExifRational 0 10),
			(meteringMode, ExifNumber 5),
			(lightSource, ExifNumber 0),
			(fileSource, ExifUndefined ""),
			(flash, ExifNumber 24),
			(focalLength, ExifRational 0 10),
			(userComment, ExifUndefined "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"),
			(flashPixVersion, ExifUndefined "010"),
			(colorSpace, ExifNumber 1),
			(sceneType, ExifUndefined ""),
			(exifImageWidth, ExifNumber 1),
			(exifImageHeight, ExifNumber 1),
			(exifInteroperabilityOffset, ExifNumber 36640),
			(customRendered, ExifNumber 0),
			(exposureMode, ExifNumber 0),
			(whiteBalance, ExifNumber 0),
			(digitalZoomRatio, ExifRational 16 16),
			(focalLengthIn35mmFilm, ExifNumber 0),
			(contrast, ExifNumber 0),
			(saturation, ExifNumber 0),
			(sceneCaptureType, ExifNumber 0),
			(sharpness, ExifNumber 0),
			(make, ExifText "SONY"),
			(model, ExifText "NEX-3N"),
			(software, ExifText "GIMP 2.8.10"),
			(orientation, ExifNumber 1),
			(imageDescription, ExifText "                               "),
			(xResolution, ExifRational 350 1),
			(yResolution, ExifRational 350 1),
			(resolutionUnit, ExifNumber 2),
			(dateTime, ExifText "2014:04:10 20:14:20"),
			(yCbCrPositioning, ExifNumber 2),
			(exifIfdOffset, ExifNumber 358),
			(printImageMatching, ExifUndefined "PrintIM\NUL0300\NUL\NUL\ETX\NUL\STX\NUL\SOH\NUL\NUL\NUL\ETX\NUL\"\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\t\DC1\NUL\NUL\DLE'\NUL\NUL\v\SI\NUL\NUL\DLE'\NUL\NUL\151\ENQ\NUL\NUL\DLE'\NUL\NUL\176\b\NUL\NUL\DLE'\NUL\NUL\SOH\FS\NUL\NUL\DLE'\NUL\NUL^\STX\NUL\NUL\DLE'\NUL\NUL\139\NUL\NUL\NUL\DLE'\NUL\NUL\203\ETX\NUL\NUL\DLE'\NUL\NUL\229\ESC\NUL\NUL\DLE'\NUL")
		]) cleanedParsed
	-- the sony maker note is 35k!! Just test its size and that it starts with "SONY DSC".
	assertEqual' 35698 (BS.length makerNoteV)
	assertEqual' "SONY DSC" (take 8 $ fmap (chr . fromIntegral) $ BS.unpack makerNoteV)
	where
		-- the makerNote is HUGE. so test it separately.
		parsed = (\(Right x) -> x) $ parseExif imageContents
		cleanedParsed = Map.fromList $ filter (\(a,_) -> not (a==makerNote)) $ Map.toList parsed
		makerNoteV = (\(Just (ExifUndefined x)) -> x) $ Map.lookup makerNote parsed

testDate :: Map ExifTag ExifValue -> Spec
testDate exifData = it "extracts the date correctly" $
	assertEqual' (Just $ LocalTime (fromGregorian 2013 10 2) (TimeOfDay 20 33 33))
		(getDateTimeOriginal exifData)

testOrientation :: Map ExifTag ExifValue -> Spec
testOrientation exifData = it "reads exif orientation" $
	assertEqual' (Just Normal) $ getOrientation exifData

testReadExifDateTime :: Spec
testReadExifDateTime = it "reads exif date time" $ do
	assertEqual' (Just $ LocalTime (fromGregorian 2013 10 2) (TimeOfDay 20 33 33)) (readExifDateTime "2013:10:02 20:33:33")
	assertEqual' Nothing (readExifDateTime "2013:10:02 20:33:3")

testReadGpsLatLong :: Map ExifTag ExifValue -> Spec
testReadGpsLatLong exifData = it "reads gps latitude longitude" $ do
	let (Just (lat,long)) = readGpsLatitudeLongitude exifData
	assertBool' $ 50.2179 < lat && 50.2180 > lat
	assertBool' $ -5.031 > long && -5.032 < long 

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"

assertBool' :: Bool -> Assertion
assertBool' = assertBool "doesn't match"
