module Suma_de_cadenas_Spec (main, spec) where

import Suma_de_cadenas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SumaCadena1" $ specG sumaCadenas1
  describe "SumaCadena2" $ specG sumaCadenas2
  describe "SumaCadena3" $ specG sumaCadenas3
  describe "SumaCadena4" $ specG sumaCadenas4

specG :: (String -> String -> String) -> Spec
specG sumaCadenas = do
  it "e1" $
    sumaCadenas "2" "6" `shouldBe` "8"
  it "e2" $
    sumaCadenas "14" "2" `shouldBe` "16"
  it "e3" $
    sumaCadenas "14" "-5" `shouldBe` "9"
  it "e4" $
    sumaCadenas "-14" "-5" `shouldBe` "-19"
  it "e5" $
    sumaCadenas "5" "-5" `shouldBe` "0"
  it "e6" $
    sumaCadenas "" "5" `shouldBe` "5"
  it "e7" $
    sumaCadenas "6" "" `shouldBe` "6"
  it "e8" $
    sumaCadenas "" "" `shouldBe` "0"
