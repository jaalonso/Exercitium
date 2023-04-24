module Pol_Comprobacion_de_raices_de_polinomios_Spec (main, spec) where

import Pol_Comprobacion_de_raices_de_polinomios
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    esRaiz 0 ejPol `shouldBe` True
  it "e2" $
    esRaiz 1 ejPol `shouldBe` False
  where
    ejPol = consPol 4 6 (consPol 1 2 polCero)
