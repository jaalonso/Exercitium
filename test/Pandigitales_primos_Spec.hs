module Pandigitales_primos_Spec (main, spec) where

import Pandigitales_primos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: [Int] -> Spec
specG pandigitalesPrimos = do
  it "e1" $
    take 3 pandigitalesPrimos       `shouldBe`  [7652413,7642513,7641253]
  it "e2" $
    2143 `elem` pandigitalesPrimos  `shouldBe`  True
  it "e3" $
    length pandigitalesPrimos       `shouldBe`  538

spec :: Spec
spec = do
  -- describe "def. 1" $ specG pandigitalesPrimos1
  describe "def. 2" $ specG pandigitalesPrimos2
  describe "def. 3" $ specG pandigitalesPrimos3
