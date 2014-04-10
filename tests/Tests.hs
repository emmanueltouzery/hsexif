import Test.Hspec
import Test.HUnit

import Graphics.HsExif
import qualified Data.ByteString as B

main :: IO ()
main = do
	imageContents <- B.readFile "tests/test.jpg"
	png <- B.readFile "tests/test.png"
	hspec $ do
		describe "not a JPG" $ testNotAJpeg png
		describe "basic parsing" $ testBasic imageContents

testNotAJpeg :: B.ByteString -> Spec
testNotAJpeg imageContents = it "returns empty list if not a JPEG" $
	assertEqual "doesn't match" [] (parseExif imageContents)

testBasic :: B.ByteString -> Spec
testBasic imageContents = it "parses a simple JPEG" $
	assertEqual "doesn't match" [] (parseExif imageContents)
