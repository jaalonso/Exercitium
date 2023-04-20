module Pol_Producto_polinomios_Spec (main, spec) where

import Pol_Producto_polinomios
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (multPol ejPol1 ejPol2) `shouldBe`
    "3*x^9 + -5*x^7 + 15*x^6 + 15*x^5 + -25*x^4 + -20*x^3 + 15*x^2 + 12*x"
  where
    ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
    ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
