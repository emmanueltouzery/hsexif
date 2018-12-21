[![Tests][circleci-image]][circleci-url]

An exif parsing library in pure haskell.

You can check the api docs on the hackage page: http://hackage.haskell.org/package/hsexif

The license is BSD3.

When building on windows if you have trouble with the `iconv` library you can also build without it. That way you loose nice decoding of the EXIF user comments though.

    cabal install -f-iconv

NB: the test images were also modified with
exiv2 -M"set Exif.Photo.UserComment charset=Unicode Test Exif commentčšž" test.jpg
to get user comments.

[circleci-image]: https://circleci.com/gh/emmanueltouzery/hsexif.svg?style=shield
[circleci-url]: https://circleci.com/gh/emmanueltouzery/hsexif
