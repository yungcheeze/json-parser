-- | 

module JsonParserSpec where

import JsonParser
import Test.Hspec

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
