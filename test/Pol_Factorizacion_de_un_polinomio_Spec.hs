module Pol_Factorizacion_de_un_polinomio_Spec (main, spec) where

import Pol_Factorizacion_de_un_polinomio
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    map show (factorizacion ejPol1)
      `shouldBe` ["1*x","1*x + 1","x^3 + -1*x^2 + 1*x + 4"]
  it "e2" $
    map show (factorizacion ejPol2)
      `shouldBe` ["1*x + -1","1*x + 1","1*x + 2","1"]
  where
    ejPol1 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
    ejPol2 = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
