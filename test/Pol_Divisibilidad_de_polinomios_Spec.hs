module Pol_Divisibilidad_de_polinomios_Spec (main, spec) where

import Pol_Divisibilidad_de_polinomios
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    divisiblePol pol1 pol2 `shouldBe` True
  it "e2" $
    divisiblePol pol1 pol3 `shouldBe` False
  where
    pol1 = consPol 2 8 (consPol 1 14 (consPol 0 3 polCero))
    pol2 = consPol 1 2 (consPol 0 3 polCero)
    pol3 = consPol 2 6 (consPol 1 2 polCero)
