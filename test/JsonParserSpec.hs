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


functorIdProp :: (Eq a) => Parser a -> String -> Bool
functorIdProp parser input =
  runParser (fmap id parser) input == id (runParser parser input)

functorComposeProp :: (Eq c) => Parser a -> (b -> c) -> (a -> b) -> String -> Bool
functorComposeProp parser f g input   =
  runParser (fmap (f . g) parser) input
    == runParser ((fmap f) . (fmap g) $ parser) input
