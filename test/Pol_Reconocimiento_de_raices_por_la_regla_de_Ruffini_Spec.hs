module Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini_Spec (main, spec) where

import Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    esRaizRuffini 0 ejPol `shouldBe` True
  it "e2" $
    esRaizRuffini 1 ejPol `shouldBe` False
  where
    ejPol = consPol 4 6 (consPol 1 2 polCero)
