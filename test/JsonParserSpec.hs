-- | 

module JsonParserSpec where

import           JsonParser
import           Test.Hspec
import           Test.QuickCheck
import           Data.Char

spec :: Spec
spec = do
  describe "charP" $ do
    let xParser = runParser $ charP 'x'
    context "succeeds when" $ do
      it "first char matches" $
        xParser "x" `shouldBe` Just ("", 'x')
    context "fails when" $ do
      it "first char doesn't match" $
        xParser "yx" `shouldBe` Nothing
      it "input string is empty" $
        xParser "" `shouldBe` Nothing
  describe  "Functor Parser" $ do
    it "fmap id = id" $
      property (functorIdProp $ charP 'x')
    it "fmap (f . g) == fmap f . fmap g" $
      property (functorComposeProp (charP  'x') (+1) ord)
  describe "Applicative Parser" $ do
    let xParser = charP 'x'
    it "pure f <*> x = fmap f x" $
      property (applicativeLaw1 xParser ord)
    it "pure id <*> x = x" $
      property (applicativeLaw2 xParser)
    it "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $
      property (applicativeLaw3 (pure (+1)) (pure ord) xParser)
    it "pure f <*> pure x = pure (f x)" $
      property (applicativeLaw4 ord 'x')
    it "u <*> pure y = pure ($ y) <*> u" $
      property (applicativeLaw5 (pure ord) 'x')
  describe "stringP" $ do
    context "succeeds when" $ do
      it "leading string matches" $
        runParser (stringP "hello") "hello" `shouldBe` Just ("", "hello")
      it "leaves remaining input" $
        runParser (stringP "hello") "helloxxx" `shouldBe` Just ("xxx", "hello")
    context "fails when" $ do
      it "leading string doesn't match" $
        runParser (stringP "hello") "xxx" `shouldBe` Nothing
      it "input empty" $
        runParser (stringP "hello") "" `shouldBe` Nothing
      it "incomplete match" $
        runParser (stringP "hello") "hell" `shouldBe` Nothing
      it "partial match" $
        runParser (stringP "hello") "hellxxx" `shouldBe` Nothing
  describe "jsonNull" $ do
    it "parses null" $
      runParser jsonNull "null" `shouldBe` Just ("", JsonNull)
    it "fails if not null" $
      runParser jsonNull "xxx" `shouldBe` Nothing
    it "fails if input short" $
      runParser jsonNull "nul" `shouldBe` Nothing



functorIdProp :: (Eq a) => Parser a -> String -> Bool
functorIdProp parser input =
  runParser (fmap id parser) input == id (runParser parser input)

functorComposeProp :: (Eq c) => Parser a -> (b -> c) -> (a -> b) -> String -> Bool
functorComposeProp parser f g =
  parserEq (fmap (f . g) parser) (fmap f . fmap g $ parser)

applicativeLaw1 :: (Eq b) => Parser a -> (a -> b) -> String -> Bool
applicativeLaw1 parser f =
  parserEq (pure f <*> parser) (fmap f parser)

applicativeLaw2 :: (Eq a) => Parser a -> String -> Bool
applicativeLaw2 parser =
  parserEq (pure id <*> parser) parser

applicativeLaw3 :: (Eq c) => Parser (b -> c) -> Parser (a -> b) -> Parser a -> String -> Bool
applicativeLaw3 u v w = parserEq (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeLaw4 :: (Eq b) => (a -> b) -> a -> String -> Bool
applicativeLaw4 f x = parserEq (pure f <*> pure x) (pure (f x))

applicativeLaw5 :: Eq b => Parser (a -> b) -> a -> String -> Bool
applicativeLaw5 u y = parserEq (u <*> pure y) (pure ($ y) <*> u)

parserEq :: Eq a => Parser a -> Parser a -> String -> Bool
parserEq parserA parserB input = runParser parserA input == runParser parserB input
