module Pol_Derivada_de_un_polinomio_Spec (main, spec) where

import Pol_Derivada_de_un_polinomio
import TAD.Polinomio (polCero, consPol)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (derivada ejPol) `shouldBe` "5*x^4 + 10*x + 4"
  where
    ejPol = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
