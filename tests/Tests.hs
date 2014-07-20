{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.HUnit

import Graphics.HsExif
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Char (chr)
import Control.Monad (liftM)
import Data.List

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
		describe "read exif date time" testReadExifDateTime
		describe "read GPS lat long" $ testReadGpsLatLong gpsExifData
		describe "read GPS lat long -- no data" $ testReadGpsLatLongNoData exifData
		describe "test formatAsFloatingPoint" testFormatAsFloatingPoint
		describe "pretty printing" $ testPrettyPrint gpsExifData exifData
		describe "flash fired" $ testFlashFired exifData

testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
	assertEqual' (Left "Not a JPEG file") (parseExif imageContents)

testNoExif :: B.ByteString -> Spec
testNoExif imageContents = it "returns empty list if no EXIF" $
	assertEqual' (Left "No EXIF in JPEG") (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $ do
	assertEqualListDebug 
		(sort [
			(exposureTime, ExifRational 1 160),
			(fnumber, ExifRational 0 10),
			(exposureProgram, ExifNumber 2),
			(isoSpeedRatings, ExifNumber 1600),
			(exifVersion, ExifUndefined "0230"),
			(dateTimeOriginal, ExifText "2013:10:02 20:33:33"),
			(dateTimeDigitized, ExifText "2013:10:02 20:33:33"),
			(componentConfiguration, ExifUndefined "\SOH\STX\ETX\NUL"),
			(compressedBitsPerPixel, ExifRational 2 1),
			(brightnessValue, ExifRational (-4226) 2560),
			(exposureBiasValue, ExifRational 0 10),
			(maxApertureValue, ExifRational 0 10),
			(meteringMode, ExifNumber 5),
			(lightSource, ExifNumber 0),
			(flash, ExifNumber 24),
			(focalLength, ExifRational 0 10),
			(flashPixVersion, ExifUndefined "0100"),
			(colorSpace, ExifNumber 1),
			(sceneType, ExifUndefined "\SOH"),
			(exifImageWidth, ExifNumber 1),
			(exifImageHeight, ExifNumber 1),
			(exifInteroperabilityOffset, ExifNumber 36616),
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
			(userComment, ExifUndefined "UNICODE\NULT\NULe\NULs\NULt\NUL \NULE\NULx\NULi\NULf\NUL \NULc\NULo\NULm\NULm\NULe\NULn\NULt\NUL\r\SOHa\SOH~\SOH"),
			(dateTime, ExifText "2014:04:10 20:14:20"),
			(yCbCrPositioning, ExifNumber 2),
			(fileSource, ExifUndefined "\ETX"),
			(exifIfdOffset, ExifNumber 358),
			(printImageMatching, ExifUndefined "PrintIM\NUL0300\NUL\NUL\ETX\NUL\STX\NUL\SOH\NUL\NUL\NUL\ETX\NUL\"\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\t\DC1\NUL\NUL\DLE'\NUL\NUL\v\SI\NUL\NUL\DLE'\NUL\NUL\151\ENQ\NUL\NUL\DLE'\NUL\NUL\176\b\NUL\NUL\DLE'\NUL\NUL\SOH\FS\NUL\NUL\DLE'\NUL\NUL^\STX\NUL\NUL\DLE'\NUL\NUL\139\NUL\NUL\NUL\DLE'\NUL\NUL\203\ETX\NUL\NUL\DLE'\NUL\NUL\229\ESC\NUL\NUL\DLE'\NUL\NUL")
		]) (sort $ Map.toList cleanedParsed)
	-- the sony maker note is 35k!! Just test its size and that it starts with "SONY DSC".
	assertEqual' 35692 (BS.length makerNoteV)
	assertEqual' "SONY DSC" (take 8 $ fmap (chr . fromIntegral) $ BS.unpack makerNoteV)
	where
		-- the makerNote is HUGE. so test it separately.
		parsed = (\(Right x) -> x) $ parseExif imageContents
		cleanedParsed = Map.fromList $ filter (\(a,_) -> (a/=makerNote)) $ Map.toList parsed
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
	let (Just (lat,long)) = getGpsLatitudeLongitude exifData
	assertBool' $ 50.2179 < lat && 50.2180 > lat
	assertBool' $ -5.031 > long && -5.032 < long 

testReadGpsLatLongNoData :: Map ExifTag ExifValue -> Spec
testReadGpsLatLongNoData exifData = it "reads gps latitude longitude" $
	assertEqual' Nothing $ getGpsLatitudeLongitude exifData

testFormatAsFloatingPoint :: Spec
testFormatAsFloatingPoint = it "properly formats as floating point" $ do
	assertEqual' "0.75" $ formatAsFloatingPoint 2 $ ExifRational 3 4
	assertEqual' "0.75, -0.50, 0.25" $ formatAsFloatingPoint 2 $ ExifRationalList [(3,4),(-1,2),(1,4)]

testPrettyPrint :: Map ExifTag ExifValue -> Map ExifTag ExifValue -> Spec
testPrettyPrint exifData stdExifData = it "pretty prints many tags properly" $ do
	checkPrettyPrinter orientation "Top-left" exifData
	checkPrettyPrinter flash "Flash did not fire, auto mode" exifData
	checkPrettyPrinter exposureTime "1/200 sec." exifData
	checkPrettyPrinter maxApertureValue "f/2.0" exifData
	checkPrettyPrinter exposureProgram "Normal program" exifData
	checkPrettyPrinter exifVersion "Exif version 2.30" exifData
	checkPrettyPrinter componentConfiguration "YCbCr-" exifData
	checkPrettyPrinter exposureBiasValue "0.00 EV" exifData
	checkPrettyPrinter meteringMode "Pattern" exifData
	checkPrettyPrinter lightSource "Unknown" exifData
	checkPrettyPrinter flashPixVersion "FlashPix version 1.0" exifData
	checkPrettyPrinter colorSpace "sRGB" exifData
	checkPrettyPrinter customRendered "Normal process" exifData
	checkPrettyPrinter exposureMode "Auto exposure" exifData
	checkPrettyPrinter whiteBalance "Auto white balance" exifData
	checkPrettyPrinter sceneCaptureType "Standard" exifData
	checkPrettyPrinter gainControl "Normal" exifData
	checkPrettyPrinter contrast "Normal" exifData
	checkPrettyPrinter saturation "Normal" exifData
	checkPrettyPrinter resolutionUnit "Inch" exifData
	checkPrettyPrinter yCbCrPositioning "Co-sited" exifData
	checkPrettyPrinter gpsLatitude "50.217917" exifData
	checkPrettyPrinter userComment "Test Exif comment" exifData
	checkPrettyPrinter userComment "Test Exif commentčšž" stdExifData

checkPrettyPrinter :: ExifTag -> Text -> Map ExifTag ExifValue -> Assertion
checkPrettyPrinter tag str exifData = assertEqual' (Just str) $ liftM (prettyPrinter tag) $ Map.lookup tag exifData

testFlashFired :: Map ExifTag ExifValue -> Spec
testFlashFired exifData = it "properly reads whether the flash was fired" $ do
	assertEqual' (Just False) $ wasFlashFired exifData
	assertEqual' Nothing $ wasFlashFired Map.empty
	assertEqual' (Just False) $ wasFlashFired $ makeExifMapWithFlash 0
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 1
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 5
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 7
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 9
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x0D
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x0F
	assertEqual' (Just False) $ wasFlashFired $ makeExifMapWithFlash 0x10
	assertEqual' (Just False) $ wasFlashFired $ makeExifMapWithFlash 0x18
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x19
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x1D
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x1F
	assertEqual' (Just False) $ wasFlashFired $ makeExifMapWithFlash 0x20
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x41
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x45
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x47
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x4D
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x4F
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x59
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x5D
	assertEqual' (Just True) $ wasFlashFired $ makeExifMapWithFlash 0x5F

makeExifMapWithFlash :: Int -> Map ExifTag ExifValue
makeExifMapWithFlash flashV = Map.fromList [(flash, ExifNumber flashV)]

assertEqualListDebug :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertEqualListDebug = assertEqualListDebug' (0 :: Int)
	where
		assertEqualListDebug' idx (x:xs) (y:ys) = do
			assertEqual ("index " ++ show idx ++ " differs: " ++ show x ++ " /= " ++ show y) x y
			assertEqualListDebug' (idx+1) xs ys
		assertEqualListDebug' _ (x:_) [] = assertBool ("List lengths differ, expected " ++ show x) False
		assertEqualListDebug' _ [] (y:_) = assertBool ("List lengths differ, got " ++ show y) False
		assertEqualListDebug' _ [] [] = assertBool "" True

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"

assertBool' :: Bool -> Assertion
assertBool' = assertBool "doesn't match"
