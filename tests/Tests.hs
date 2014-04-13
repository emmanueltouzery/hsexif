import Test.Hspec
import Test.HUnit

import Graphics.HsExif
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Time.Calendar

main :: IO ()
main = do
	imageContents <- B.readFile "tests/test.jpg"
	noExif <- B.readFile "tests/noexif.jpg"
	png <- B.readFile "tests/test.png"
	hspec $ do
		describe "not a JPG" $ testNotAJpeg png
		describe "no EXIF" $ testNoExif noExif
		describe "basic parsing" $ testBasic imageContents
		describe "extract picture date" $ testDate imageContents

testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
	assertEqual "doesn't match" (Left "Not a JPEG file") (parseExif imageContents)

testNoExif :: B.ByteString -> Spec
testNoExif imageContents = it "returns empty list if no EXIF" $
	assertEqual "doesn't match" (Left "No EXIF in JPEG") (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $
	assertEqual "doesn't match" (Right $ Map.fromList [(ExposureTime,"1/160"),(FNumber,"0/10"),(ExposureProgram,"2"),(ISOSpeedRatings,"1600"),(ExifVersion,"808661552"),(DateTimeOriginal,"2013:10:02 20:33:33"),(DateTimeDigitized,"2013:10:02 20:33:33"),(ComponentConfiguration,"197121"),(CompressedBitsPerPixel,"2/1"),(BrightnessValue,"-4226/2560"),(ExposureBiasValue,"0/10"),(MaxApertureValue,"0/10"),(MeteringMode,"5"),(LightSource,"0"),(Flash,"24"),(FocalLength,"0/10"),(MakerNote,"868"),(UserComment,"36568"),(FlashPixVersion,"808464688"),(ColorSpace,"1"),(ExifImageWidth,"1"),(ExifImageHeight,"1"),(ExifInteroperabilityOffset,"36640"),(FileSource,"3"),(SceneType,"1"),(Unknown 41985,"0"),(Unknown 41986,"0"),(Unknown 41987,"0"),(Unknown 41988,"16/16"),(Unknown 41989,"0"),(Unknown 41990,"0"),(Unknown 41992,"0"),(Unknown 41993,"0"),(Unknown 41994,"0")]) (parseExif imageContents)

testDate :: B.ByteString -> Spec
testDate imageContents = it "extracts the date correctly" $
	assertEqual "doesn't match" (Just $ LocalTime (fromGregorian 2013 10 2) (TimeOfDay 20 33 33))
		(getDateTimeOriginal $ (\(Right x) -> x) $ parseExif imageContents)
