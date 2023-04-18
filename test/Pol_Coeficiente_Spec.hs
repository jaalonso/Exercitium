module Pol_Coeficiente_Spec (main, spec) where

import Pol_Coeficiente
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    coeficiente 2 ejPol `shouldBe` 5
  it "e2" $
    coeficiente 3 ejPol `shouldBe` 0
  where ejPol = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
