module Polinomios_de_Bell_Spec (main, spec) where

import Polinomios_de_Bell
import I1M.PolOperaciones
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Polinomio Integer) -> Spec
specG polBell = do
  it "e1" $
    polBell 4                    `shouldBe`  polBell2 4 
  it "e2" $
    coeficiente 2 (polBell 12)   `shouldBe`  2047

spec :: Spec
spec = do
  describe "def. 1" $ specG polBell1
  describe "def. 2" $ specG polBell2
  describe "equivalencia" $ it "p1" $ property prop_polBell
