module Algoritmo_de_Euclides_del_mcd_Spec (main, spec) where

import Algoritmo_de_Euclides_del_mcd
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer -> Integer) -> Spec
specG mcd' = do
  it "e1" $
    mcd' 30 45  `shouldBe`  15
  it "e1" $
    mcd' 45 30  `shouldBe`  15

spec :: Spec
spec = do
  describe "def. 1" $ specG mcd
  describe "equivalencia" $ it "p1" $ property prop_mcd
