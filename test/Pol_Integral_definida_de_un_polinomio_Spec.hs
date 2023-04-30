module Pol_Integral_definida_de_un_polinomio_Spec (main, spec) where

import Pol_Integral_definida_de_un_polinomio
import TAD.Polinomio
import Data.Ratio
import Test.Hspec

main :: IO ()
main = hspec spec

ejPol :: (Num a, Eq a) => Polinomio a
ejPol = consPol 7 2 (consPol 4 5 (consPol 2 5 polCero))

spec :: Spec
spec = do
  it "e1" $
    integralDef ejPol 0 1 `shouldBe` 2.916666666666667
  it "e2" $
    (integralDef ejPol 0 1 :: Rational) `shouldBe` (35 % 12)
