import Test.Hspec
import Test.HUnit

import Graphics.HsExif
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar

main :: IO ()
main = do
	imageContents <- B.readFile "tests/test.jpg"
	noExif <- B.readFile "tests/noexif.jpg"
	png <- B.readFile "tests/test.png"
	let parseExifCorrect = (\(Right x) -> x) . parseExif
	let exifData = parseExifCorrect imageContents
	hspec $ do
		describe "not a JPG" $ testNotAJpeg png
		describe "no EXIF" $ testNoExif noExif
		describe "basic parsing" $ testBasic imageContents
		describe "extract picture date" $ testDate exifData
		describe "image orientation" $ testOrientation exifData
		describe "read exif date time" $ testReadExifDateTime

testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
	assertEqual' (Left "Not a JPEG file") (parseExif imageContents)

testNoExif :: B.ByteString -> Spec
testNoExif imageContents = it "returns empty list if no EXIF" $
	assertEqual' (Left "No EXIF in JPEG") (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $
	assertEqual' (Right $ Map.fromList [
		(exposureTime, ExifRational 1 160),
		(fnumber, ExifRational 0 10),
		(exposureProgram, ExifNumber 2),
		(isoSpeedRatings, ExifNumber 1600),
		(exifVersion, ExifNumber 808661552),
		(dateTimeOriginal, ExifText "2013:10:02 20:33:33"),
		(dateTimeDigitized, ExifText "2013:10:02 20:33:33"),
		(componentConfiguration, ExifNumber 197121),
		(compressedBitsPerPixel, ExifRational 2 1),
		(brightnessValue, ExifRational (-4226) 2560),
		(exposureBiasValue, ExifRational 0 10),
		(maxApertureValue, ExifRational 0 10),
		(meteringMode, ExifNumber 5),
		(lightSource, ExifNumber 0),
		(fileSource, ExifNumber 3),
		(flash, ExifNumber 24),
		(focalLength, ExifRational 0 10),
		(makerNote, ExifNumber 868),
		(userComment, ExifNumber 36568),
		(flashPixVersion, ExifNumber 808464688),
		(colorSpace, ExifNumber 1),
		(sceneType, ExifNumber 1),
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
		(printImageMatching, ExifNumber 252)
	]) (parseExif imageContents)

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

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"
