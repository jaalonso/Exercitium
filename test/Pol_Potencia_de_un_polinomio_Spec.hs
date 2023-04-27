module Pol_Potencia_de_un_polinomio_Spec (main, spec) where

import Pol_Potencia_de_un_polinomio
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (potencia ejPol 2) `shouldBe` "4*x^2 + 12*x + 9"
  it "e2" $
    show (potencia ejPol 3) `shouldBe` "8*x^3 + 36*x^2 + 54*x + 27"
  it "e3" $
    show (potencia2 ejPol 2) `shouldBe` "4*x^2 + 12*x + 9"
  it "e4" $
    show (potencia2 ejPol 3) `shouldBe` "8*x^3 + 36*x^2 + 54*x + 27"
  where
    ejPol = consPol 1 2 (consPol 0 3 polCero)
