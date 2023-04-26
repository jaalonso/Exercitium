module Pol_Resta_de_polinomios_Spec (main, spec) where

import Pol_Resta_de_polinomios
import TAD.Polinomio (polCero, consPol)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (restaPol ejPol1 ejPol2) `shouldBe` "x^5 + 2*x^4 + 6"
  where
    ejPol1 = consPol 5 1 (consPol 4 5 (consPol 2 5 (consPol 0 9 polCero)))
    ejPol2 = consPol 4 3 (consPol 2 5 (consPol 0 3 polCero))
