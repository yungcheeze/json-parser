-- | 

module JsonParserSpec where

import           JsonParser
import           Test.Hspec
import           Test.QuickCheck
import           Data.Char
import           Control.Applicative
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
  describe "Alternative Parser" $ do
    let xParser = charP 'x'
    it "empty <|> u = u" $
      property (alternativeLaw1 xParser)
    it "u <|> empty = u" $
      property (alternativeLaw2 xParser)
    it "u <|> (v <|> w)  =  (u <|> v) <|> w" $
      property (alternativeLaw3  (charP 'x') (charP 'y') (charP 'z'))
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
  describe "jsonValue" $ do
    it "parses null" $
      runParser jsonValue "null" `shouldBe` Just ("", JsonNull)
    it "fails if not null" $
      runParser jsonValue "xxx" `shouldBe` Nothing
    it "fails if input short" $
      runParser jsonValue "nul" `shouldBe` Nothing
  describe "jsonValue" $ do
    it "parses true" $
      runParser jsonValue "true" `shouldBe` Just ("", JsonBool True)
    it "parses true and leaves remaining input" $
      runParser jsonValue "truexxx" `shouldBe` Just ("xxx", JsonBool True)
    it "parses false" $
      runParser jsonValue "false" `shouldBe` Just ("", JsonBool False)
    it "parses false and leaves remaining input" $
      runParser jsonValue "falsexxx" `shouldBe` Just ("xxx", JsonBool False)
  describe "numberP" $ do
    it "parses number" $
      runParser numberP "123" `shouldBe` Just ("", "123")
    it "leaves remaining input" $
      runParser numberP "123xxx" `shouldBe` Just ("xxx", "123")
    it "fails on non number" $
      runParser numberP "xxx" `shouldBe` Nothing
    it "fails on empty input" $
      runParser numberP "" `shouldBe` Nothing
  describe "intP" $ do
    it "parses number" $
      runParser intP "123" `shouldBe` Just ("", 123)
    it "leaves remaining input" $
      runParser intP "123xxx" `shouldBe` Just ("xxx", 123)
    it "fails on non number" $
      runParser intP "xxx" `shouldBe` Nothing
    it "fails on empty input" $
      runParser intP "" `shouldBe` Nothing



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

alternativeLaw1 :: Eq a => Parser a -> String -> Bool
alternativeLaw1 u = parserEq (empty <|> u) u

alternativeLaw2 :: Eq a => Parser a -> String -> Bool
alternativeLaw2 u = parserEq (u <|> empty) u

alternativeLaw3 :: Eq a => Parser a -> Parser a -> Parser a -> String -> Bool
alternativeLaw3 u v w = parserEq (u <|> (v <|> w)) ((u <|> v) <|> w)

parserEq :: Eq a => Parser a -> Parser a -> String -> Bool
parserEq parserA parserB input = runParser parserA input == runParser parserB input
