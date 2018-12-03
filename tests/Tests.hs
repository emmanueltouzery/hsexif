{-# LANGUAGE OverloadedStrings, CPP #-}
import Test.Hspec
import Test.HUnit

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Char (chr)
import Control.Monad (join)
import Control.Applicative ( (<$>) )
import Data.List

import Graphics.Types (formatAsRational)
import Graphics.PrettyPrinters (ppExposureTime)
import Graphics.HsExif
import Graphics.Helpers

main :: IO ()
main = do
    imageContents <- B.readFile "tests/test.jpg"
    noExif <- B.readFile "tests/noexif.jpg"
    png  <- B.readFile "tests/test.png"
    gps  <- B.readFile "tests/gps.jpg"
    gps2 <- B.readFile "tests/gps2.jpg"
    gps3 <- B.readFile "tests/gps3.jpg"
    partial <- B.readFile "tests/partial_exif.jpg"
    tiff <- B.readFile "tests/RAW_NIKON_D1.NEF"
    let parseExifM   = hush . parseExif
    let exifData     = parseExifM imageContents
    let gpsExifData  = parseExifM gps
    let gps2ExifData = parseExifM gps2
    let gps3ExifData = parseExifM gps3
    let tiffExifData = parseExifM tiff
    hspec $ do
        describe "not a JPG" $ testNotAJpeg png
        describe "no EXIF" $ testNoExif noExif
        describe "basic parsing" $ testBasic imageContents
        describe "extract picture date" $ testDate exifData
        describe "image orientation" $ testOrientation exifData
        describe "read exif date time" testReadExifDateTime
        describe "read GPS lat long" $ testReadGpsLatLong gpsExifData 50.2179 (-5.031)
        describe "read GPS lat long -- no data" $ testReadGpsLatLongNoData exifData
        describe "read GPS lat long -- length 1 for rel" $
            testReadGpsLatLong gps3ExifData 38.1785 (-7.2109)
        describe "test formatAsFloatingPoint" testFormatAsFloatingPoint
        describe "test formatAsRational" testFormatAsRational
        describe "test ppExposureTime" testPpExposureTime
        describe "pretty printing" $ testPrettyPrint gpsExifData exifData gps2ExifData
        describe "flash fired" $ testFlashFired exifData
        describe "partial exif data" $ testPartialExif partial
        describe "tiff file" $ testNef tiffExifData

        describe "unusual data layouts" $ do
            it "parses EXIF below IDF0" $ do
                result <- parseFileExif "tests/test-exif-below-idf0.jpg"
                case result of
                    Left err -> assertBool ("Cannot parse: " ++ err) False
                    Right tags -> do
                        (Map.lookup dateTimeOriginal tags) `assertEqual'` (Just $ ExifText "2013:10:02 20:33:33") 


testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
    assertEqual' (Left "Not a JPEG, TIFF, RAF, or TIFF-based raw file") (parseExif imageContents)

testNoExif :: B.ByteString -> Spec
testNoExif imageContents = it "returns empty list if no EXIF" $
    assertEqual' (Left "No EXIF in JPEG") (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $ do
    case cleanedParsedM of
        Nothing -> assertBool "Parsing fails" False
        Just cleanedParsed -> assertEqualListDebug
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
                (printImageMatching, ExifUndefined "PrintIM\NUL0300\NUL\NUL\ETX\NUL\STX\NUL\SOH\NUL\NUL\NUL\ETX\NUL\"\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\t\DC1\NUL\NUL\DLE'\NUL\NUL\v\SI\NUL\NUL\DLE'\NUL\NUL\151\ENQ\NUL\NUL\DLE'\NUL\NUL\176\b\NUL\NUL\DLE'\NUL\NUL\SOH\FS\NUL\NUL\DLE'\NUL\NUL^\STX\NUL\NUL\DLE'\NUL\NUL\139\NUL\NUL\NUL\DLE'\NUL\NUL\203\ETX\NUL\NUL\DLE'\NUL\NUL\229\ESC\NUL\NUL\DLE'\NUL\NUL")
            ]) (sort cleanedParsed)
    -- the sony maker note is 35k!! Just test its size and that it starts with "SONY DSC".
    assertEqual' (Just 35692) (BS.length <$> makerNoteV)
    assertEqual' (Just "SONY DSC") (take 8 <$> fmap (chr . fromIntegral) <$> BS.unpack <$> makerNoteV)
    where
        -- the makerNote is HUGE. so test it separately.
        parsed = hush $ parseExif imageContents
        cleanedParsedM = filter ((/=makerNote) . fst) <$> (Map.toList <$> parsed)
        makerNoteV = parsed >>= Map.lookup makerNote >>= getUndefMaybe

getUndefMaybe :: ExifValue -> Maybe BS.ByteString
getUndefMaybe (ExifUndefined x) = Just x
getUndefMaybe _ = Nothing

testDate :: Maybe (Map ExifTag ExifValue) -> Spec
testDate exifData = it "extracts the date correctly" $
    assertEqual' (Just $ LocalTime (fromGregorian 2013 10 2) (TimeOfDay 20 33 33))
        (exifData >>= getDateTimeOriginal)

testOrientation :: Maybe (Map ExifTag ExifValue) -> Spec
testOrientation exifData = it "reads exif orientation" $
    assertEqual' (Just Normal) $ exifData >>= getOrientation

testReadExifDateTime :: Spec
testReadExifDateTime = it "reads exif date time" $ do
    assertEqual' (Just $ LocalTime (fromGregorian 2013 10 2) (TimeOfDay 20 33 33)) (readExifDateTime "2013:10:02 20:33:33")
    assertEqual' Nothing (readExifDateTime "2013:10:02 20:33:3")

testReadGpsLatLong :: Maybe (Map ExifTag ExifValue) -> Double -> Double -> Spec
testReadGpsLatLong exifData xLat xLong = it "reads gps latitude longitude" $ do
    let (Just (lat,long)) = exifData >>= getGpsLatitudeLongitude
    let equalEnough a b = assertBool (show a ++ " too different from " ++ show b ) $
                          abs (a - b) < 0.001
    equalEnough lat xLat
    equalEnough long xLong

testReadGpsLatLongNoData :: Maybe (Map ExifTag ExifValue) -> Spec
testReadGpsLatLongNoData exifData = it "reads gps latitude longitude" $
    assertEqual' Nothing $ join $ getGpsLatitudeLongitude <$> exifData

testFormatAsFloatingPoint :: Spec
testFormatAsFloatingPoint = it "properly formats as floating point" $ do
    assertEqual' "0.75" $ formatAsFloatingPoint 2 $ ExifRational 3 4
    assertEqual' "0.75, -0.50, 0.25" $ formatAsFloatingPoint 2 $ ExifRationalList [(3,4),(-1,2),(1,4)]

testFormatAsRational :: Spec
testFormatAsRational = it "properly formats as rational" $ do
    assertEqual' "3/4" $ formatAsRational $ ExifRational 6 8
    assertEqual' "2" $ formatAsRational $ ExifRational 8 4
    assertEqual' "3/4, -1/2, 1/4" $ formatAsRational $ ExifRationalList [(3,4),(-1,2),(1,4)]

testPpExposureTime :: Spec
testPpExposureTime = it "properly formats exposure time" $ do
    assertEqual'   "1.3 sec." $ ppExposureTime (ExifRational 13 10)
    assertEqual'     "8 sec." $ ppExposureTime (ExifRational 8 1)
    assertEqual' "1/125 sec." $ ppExposureTime (ExifRational 10 1250)
    assertEqual' "1/300 sec." $ ppExposureTime (ExifRational 10 3000)
    assertEqual'   "0.8 sec." $ ppExposureTime (ExifRational 10 13)

testPrettyPrint :: Maybe (Map ExifTag ExifValue)
    -> Maybe (Map ExifTag ExifValue)
    -> Maybe (Map ExifTag ExifValue) -> Spec
testPrettyPrint exifData stdExifData gps2ExifData = it "pretty prints many tags properly" $ do
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
    checkPrettyPrinter gpsLatitude "50° 13' 4.50\"" exifData
    checkPrettyPrinter gpsLatitudeRef "North" exifData
    checkPrettyPrinter gpsAltitude "2681.1111" gps2ExifData
    checkPrettyPrinter gpsTimeStamp "09:12:32.21" gps2ExifData
    checkPrettyPrinter userComment "Test Exif comment" exifData
#if ICONV
    checkPrettyPrinter userComment "Test Exif commentčšž" stdExifData
#endif

checkPrettyPrinter :: ExifTag -> Text -> Maybe (Map ExifTag ExifValue) -> Assertion
checkPrettyPrinter tag str exifData = assertEqual' (Just str) $ prettyPrinter tag <$> (exifData >>= Map.lookup tag)

testFlashFired :: Maybe (Map ExifTag ExifValue) -> Spec
testFlashFired exifData = it "properly reads whether the flash was fired" $ do
    assertEqual' (Just False) $ exifData >>= wasFlashFired
    assertEqual' Nothing $ wasFlashFired Map.empty
    let check (ex, val) = Just ex == wasFlashFired (makeExifMapWithFlash val)
    assertBool "a number test fails" $ all check [
        (False, 0), (True, 1), (True, 5), (True, 7), (True, 9), (True, 0x0D),
        (True, 0x0F), (False, 0x10), (False, 0x18), (True, 0x19), (True, 0x1D),
        (True, 0x1F), (False, 0x20), (True, 0x41), (True, 0x45), (True, 0x47),
        (True, 0x4D), (True, 0x4F), (True, 0x59), (True, 0x5D), (True, 0x5F)]

makeExifMapWithFlash :: Int -> Map ExifTag ExifValue
makeExifMapWithFlash flashV = Map.fromList [(flash, ExifNumber flashV)]

testPartialExif :: B.ByteString -> Spec
testPartialExif imageContents = it "parses a partial exif JPEG" $
    assertEqual' (Right []) (Map.toList <$> parseExif imageContents)

testNef :: Maybe (Map ExifTag ExifValue) -> Spec
testNef Nothing = it "parses EXIF from a NEF file" $
                   assertBool "failed to parse EXIF from nef" False
testNef (Just exifMap) = it "parses EXIF from a NEF file" $ do
    let makerNoteV = Map.lookup makerNote exifMap >>= getUndefMaybe
    -- test only the length of the maker note
    assertEqual' (Just 394) (BS.length <$> makerNoteV)
    let cleanedExifMap = filter ((/=makerNote) . fst) (Map.toList exifMap)
    assertEqualListDebug
        (sort [(exposureTime, ExifRational 10 2500),
                (fnumber, ExifRational 76 10),
                (exposureProgram, ExifNumber 2),
                (dateTimeOriginal, ExifText "2000:11:19 13:01:50"),
                (dateTimeDigitized, ExifText "2000:11:19 13:01:50"),
                (exposureBiasValue, ExifRational 0 6),
                (maxApertureValue, ExifRational 30 10),
                (meteringMode, ExifNumber 5),
                (focalLength, ExifRational 200 10),
                (userComment, ExifUndefined $ BS.concat $ replicate 48 "\NUL"),
                (subSecTime, ExifText "24"),
                (subSecTimeOriginal, ExifText "24"),
                (subSecTimeDigitized, ExifText "24"),
                (sensingMethod, ExifNumber 2),
                (fileSource, ExifUndefined "\ETX"),
                (sceneType, ExifUndefined "\SOH"),
                (cfaPattern, ExifUndefined "\NUL\STX\NUL\STX\STX\SOH\SOH\NUL"),
                (ExifTag IFD0 Nothing 0xfe (T.pack . show), ExifNumber 1),
                (ExifTag IFD0 Nothing 0x100 (T.pack . show), ExifNumber 160),
                (ExifTag IFD0 Nothing 0x101 (T.pack . show), ExifNumber 120),
                (ExifTag IFD0 Nothing 0x102 (T.pack . show), ExifNumberList [8,8,8]),
                (ExifTag IFD0 Nothing 0x103 (T.pack . show), ExifNumber 1),
                (ExifTag IFD0 Nothing 0x106 (T.pack . show), ExifNumber 2),
                (imageDescription, ExifText "                               "),
                (make, ExifText "NIKON CORPORATION"),
                (model, ExifText "NIKON D1 "),
                (ExifTag IFD0 Nothing 0x111 (T.pack . show), ExifNumber 1280),
                (ExifTag IFD0 Nothing 0x115 (T.pack . show), ExifNumber 3),
                (ExifTag IFD0 Nothing 0x116 (T.pack . show), ExifNumber 120),
                (ExifTag IFD0 Nothing 0x117 (T.pack . show), ExifNumber 57600),
                (xResolution, ExifRational 300 1),
                (yResolution, ExifRational 300 1),
                (ExifTag IFD0 Nothing 0x11c (T.pack . show), ExifNumber 1),
                (resolutionUnit, ExifNumber 2),
                (software, ExifText "Ver.1.05\0"),
                (dateTime, ExifText "2000:11:19 13:01:50"),
                (ExifTag IFD0 Nothing 0x14a (T.pack . show), ExifNumber 58880),
                (referenceBlackWhite, ExifRationalList [(0,1),(255,1),(0,1),(255,1),(0,1),(255,1)]),
                (copyright, ExifText "Copyright,NIKON CORPORATION,1999\0"),
                (ExifTag IFD0 Nothing 0x9003 (T.pack . show), ExifText "2000:11:19 13:01:50"),
                (ExifTag IFD0 Nothing 0x9216 (T.pack . show), ExifNumberList [1,0,0,0])
              ])
        (sort cleanedExifMap)

assertEqualListDebug :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertEqualListDebug = assertEqualListDebug' (0 :: Int)
    where
        assertEqualListDebug' idx (x:xs) (y:ys) = do
            assertEqual ("lengths are off by " ++ show (length ys - length xs) ++
                         "; index " ++ show idx ++ " differs: " ++ show x ++ " /= " ++ show y) x y
            assertEqualListDebug' (idx+1) xs ys
        assertEqualListDebug' _ (x:r) [] = assertBool ("List lengths differ by " ++ show (length r) ++ ", expected " ++ show x) False
        assertEqualListDebug' _ [] (y:r) = assertBool ("List lengths differ by " ++ show (length r) ++ ", got " ++ show y) False
        assertEqualListDebug' _ [] [] = assertBool "" True

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"
