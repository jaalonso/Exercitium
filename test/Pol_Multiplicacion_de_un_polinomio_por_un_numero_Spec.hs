module Pol_Multiplicacion_de_un_polinomio_por_un_numero_Spec (main, spec) where

import Pol_Multiplicacion_de_un_polinomio_por_un_numero
import TAD.Polinomio
import Data.Ratio
import Test.Hspec

main :: IO ()
main = hspec spec

ejPol :: (Num a, Eq a) => Polinomio a
ejPol = consPol 1 2 (consPol 0 3 polCero)

spec :: Spec
spec = do
  it "e1" $
    show (multEscalar 4 ejPol) `shouldBe` "8*x + 12"
  it "e2" $
    show (multEscalar (1 % 4) ejPol) `shouldBe` "1 % 2*x + 3 % 4"
