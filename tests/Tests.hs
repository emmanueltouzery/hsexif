import Test.Hspec
import Test.HUnit

import Graphics.HsExif
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Word

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

testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
	assertEqual "doesn't match" (Left "Not a JPEG file") (parseExif imageContents)

testNoExif :: B.ByteString -> Spec
testNoExif imageContents = it "returns empty list if no EXIF" $
	assertEqual "doesn't match" (Left "No EXIF in JPEG") (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $
	assertEqual "doesn't match" (Right $ Map.fromList [
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
		(unknownExifSubIfdTag 40960, ExifNumber 808464688),
		(colorSpace, ExifNumber 1),
		(sceneType, ExifNumber 1),
		(exifImageWidth, ExifNumber 1),
		(exifImageHeight, ExifNumber 1),
		(unknownExifSubIfdTag 40965, ExifNumber 36640),
		(unknownExifSubIfdTag 41985, ExifNumber 0),
		(unknownExifSubIfdTag 41986, ExifNumber 0),
		(unknownExifSubIfdTag 41987, ExifNumber 0),
		(unknownExifSubIfdTag 41988, ExifRational 16 16),
		(unknownExifSubIfdTag 41989, ExifNumber 0),
		(unknownExifSubIfdTag 41990, ExifNumber 0),
		(unknownExifSubIfdTag 41992, ExifNumber 0),
		(unknownExifSubIfdTag 41993, ExifNumber 0),
		(unknownExifSubIfdTag 41994, ExifNumber 0),
		(make, ExifText "SONY"),
		(model, ExifText "NEX-3N"),
		(software, ExifText "GIMP 2.8.10"),
		(orientation, ExifNumber 1),
		(unknownExifIfd0Tag 0x10e, ExifText "                               "),
		(unknownExifIfd0Tag 0x11a, ExifRational 350 1),
		(unknownExifIfd0Tag 0x11b, ExifRational 350 1),
		(unknownExifIfd0Tag 0x128, ExifNumber 2),
		(unknownExifIfd0Tag 0x132, ExifText "2014:04:10 20:14:20"),
		(unknownExifIfd0Tag 0x213, ExifNumber 2),
		(unknownExifIfd0Tag 0x8769, ExifNumber 358),
		(unknownExifIfd0Tag 0xc4a5, ExifNumber 252)
	]) (parseExif imageContents)

unknownExifSubIfdTag :: Word16 -> ExifTag
unknownExifSubIfdTag = ExifTag ExifSubIFD Nothing

unknownExifIfd0Tag :: Word16 -> ExifTag
unknownExifIfd0Tag = ExifTag IFD0 Nothing

testDate :: Map ExifTag ExifValue -> Spec
testDate exifData = it "extracts the date correctly" $
	assertEqual "doesn't match" (Just $ LocalTime (fromGregorian 2013 10 2) (TimeOfDay 20 33 33))
		(getDateTimeOriginal exifData)

testOrientation :: Map ExifTag ExifValue -> Spec
testOrientation exifData = it "reads exif orientation" $
	assertEqual "doesn't match" (Just Normal) $ getOrientation exifData
