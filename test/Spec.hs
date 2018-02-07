{-# LANGUAGE OverloadedStrings #-}

import NMA
import SymEither

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

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

testGoodResponseParsingBadInt :: Assertion
testGoodResponseParsingBadInt =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<success code=\"200\" remaining=\"nineteen\" resettimer=\"36\" />",
                    "</nma>"
                  ] in
   parseResponse txt @?= Left (Response "" 0 36)

instance Eq a => EqProp (SymEither a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (SymEither a) where
  arbitrary = oneof [SLeft <$> arbitrary, SRight <$> arbitrary]

instance EqProp Notification where (=-=) = eq

instance EqProp PriorityLevel where (=-=) = eq

instance Arbitrary PriorityLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Notification where
  arbitrary = Notification
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

from_either_prop :: (Eq a) => a -> Either a a -> Bool
from_either_prop n l@(Left _) = fromEither n l == SLeft n
from_either_prop n r@(Right x) = fromEither n r == SRight x

to_either_prop :: (Eq a) => SymEither a -> Bool
to_either_prop l@(SLeft a) = toEither l == Left a
to_either_prop r@(SRight a) = toEither r == Right a

left_prop :: (Eq a) => SymEither a -> Bool
left_prop l@(SLeft _) = left l == l
left_prop r@(SRight a) = left r == SLeft a

someSym :: SymEither (Int, Int, Int)
someSym = undefined

-- TODO:  Fix when tasty-quickcheck 0.9.1 is available
testProperties' :: TestName -> [(String, Property)] -> TestTree
testProperties' name = testGroup name . map (uncurry testProperty)

tests :: [TestTree]
tests = [
  testCase "error response parsing" testErrorResponseParsing,
  testCase "error response parsing (402)" testErrorResponseParsing2,
  testCase "success response parsing" testGoodResponseParsing,
  testCase "success response parsing with err" testGoodResponseParsingBadInt,

  testProperties' "PriorityLevel monoid" (unbatch $ monoid (mempty :: PriorityLevel)),
  testProperties' "Notification monoid" (unbatch $ monoid (mempty :: Notification)),

  testProperties' "SymEither functor" (unbatch $ functor someSym),
  testProperties' "SymEither applicative" (unbatch $ applicative someSym),
  testProperties' "SymEither monad" (unbatch $ monad someSym),
  testProperty "SymEither fromEither" (from_either_prop 0 :: Either Int Int -> Bool),
  testProperty "SymEither toEither" (to_either_prop :: SymEither Int -> Bool),
  testProperty "SymEither left" (left_prop :: SymEither Int -> Bool)
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
