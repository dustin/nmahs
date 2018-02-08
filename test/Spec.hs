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
    parseResponse txt @?= Left (Response "invalid API keys" 401 0 0)

testErrorResponseParsing2 :: Assertion
testErrorResponseParsing2 =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<error code=\"402\" resettimer=\"13\">Your IP exceeded the maximum number of API calls per hour allowed.</error>",
                    "</nma>"
                  ] in
    parseResponse txt @?= Left (Response "Your IP exceeded the maximum number of API calls per hour allowed." 402 0 13)

testGoodResponseParsing :: Assertion
testGoodResponseParsing =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<success code=\"200\" remaining=\"19\" resettimer=\"36\" />",
                    "</nma>"
                  ] in
   parseResponse txt @?= Right (Response "" 200 19 36)

testGoodResponseParsingBadInt :: Assertion
testGoodResponseParsingBadInt =
  let txt = xpack [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<nma>",
                    "<success code=\"200\" remaining=\"nineteen\" resettimer=\"36\" />",
                    "</nma>"
                  ] in
   parseResponse txt @?= Left (Response "" 200 0 36)

instance Eq a => EqProp (SymEither a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (SymEither a) where
  arbitrary = pure <$> arbitrary

instance EqProp Notification where (=-=) = eq

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

fromEitherProp :: (Eq a) => a -> Either a a -> Bool
fromEitherProp n l@(Left _) = fromEither n l == SymEither (False, n)
fromEitherProp n r@(Right x) = fromEither n r == SymEither (True,  x)

toEitherProp :: (Eq a) => SymEither a -> Bool
toEitherProp l@(SymEither (False,a)) = toEither l == Left a
toEitherProp r@(SymEither (True, a)) = toEither r == Right a

leftProp :: (Eq a) => SymEither a -> Bool
leftProp l@(SymEither (False, _)) = left l == l
leftProp r@(SymEither (True, a)) = left r == SymEither (False, a)

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

  testProperties' "Notification monoid" (unbatch $ monoid notification),

  testProperties' "SymEither functor" (unbatch $ functor someSym),
  testProperties' "SymEither applicative" (unbatch $ applicative someSym),
  testProperties' "SymEither monad" (unbatch $ monad someSym),
  testProperty "SymEither fromEither" (fromEitherProp 0 :: Either Int Int -> Bool),
  testProperty "SymEither toEither" (toEitherProp :: SymEither Int -> Bool),
  testProperty "SymEither left" (leftProp :: SymEither Int -> Bool)
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
