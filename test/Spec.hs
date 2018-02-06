{-# LANGUAGE OverloadedStrings #-}

import NMA

import qualified Data.ByteString.Char8 as C
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

xpack :: [String] -> C.ByteString
xpack = C.pack.unlines

testErrorResponseParsing :: Assertion
testErrorResponseParsing =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<error code=\"401\">invalid API keys</error>",
                    "</nma>"
                  ] in
    parseResponse txt @?= Left (Response "invalid API keys" 0 0)

testErrorResponseParsing2 :: Assertion
testErrorResponseParsing2 =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<error code=\"402\" resettimer=\"13\">Your IP exceeded the maximum number of API calls per hour allowed.</error>",
                    "</nma>"
                  ] in
    parseResponse txt @?= Left (Response "Your IP exceeded the maximum number of API calls per hour allowed." 0 13)

testGoodResponseParsing :: Assertion
testGoodResponseParsing =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<success code=\"200\" remaining=\"19\" resettimer=\"36\" />",
                    "</nma>"
                  ] in
   parseResponse txt @?= Right (Response "" 19 36)

tests :: [TestTree]
tests = [
  testCase "error response parsing" testErrorResponseParsing,
  testCase "error response parsing (402)" testErrorResponseParsing2,
  testCase "success response parsing" testGoodResponseParsing
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
