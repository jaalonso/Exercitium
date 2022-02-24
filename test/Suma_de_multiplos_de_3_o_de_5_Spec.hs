module Suma_de_multiplos_de_3_o_de_5_Spec (main, spec) where

import Suma_de_multiplos_de_3_o_de_5
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaMultiplos = do
  it "e1" $
    sumaMultiplos 10      `shouldBe`  23
  it "e2" $
    sumaMultiplos (10^2)  `shouldBe`  2318

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaMultiplos1
  describe "def. 2" $ specG sumaMultiplos2
  describe "def. 3" $ specG sumaMultiplos3
  describe "def. 4" $ specG sumaMultiplos4
  describe "def. 5" $ specG sumaMultiplos5
  describe "def. 6" $ specG sumaMultiplos6
