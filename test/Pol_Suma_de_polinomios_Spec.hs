module Pol_Suma_de_polinomios_Spec (main, spec) where

import Pol_Suma_de_polinomios
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (sumaPol ejPol1 ejPol2) `shouldBe` "x^5 + 3*x^4 + 4*x + 3"
  where
    ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
    ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
