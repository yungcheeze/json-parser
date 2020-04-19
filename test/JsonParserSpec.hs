-- | 

module JsonParserSpec where

import JsonParser
import Test.Hspec

spec :: Spec
spec =
  describe "works" $
  it "alive" $
    True `shouldBe` True
