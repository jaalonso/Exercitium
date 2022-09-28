module Puntos_dentro_del_circulo_Spec (main, spec) where

import Puntos_dentro_del_circulo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> Int) -> Spec
specG circulo = do
  it "e1" $
    circulo 3  `shouldBe`  9
  it "e2" $
    circulo 4  `shouldBe`  15
  it "e3" $
    circulo 5  `shouldBe`  22
  it "e4" $
    circulo 100  `shouldBe`  7949

spec :: Spec
spec = do
  describe "def. 1" $ specG circulo1
  describe "def. 2" $ specG circulo2
  describe "equivalencia" $ it "p1" $ property prop_circulo
