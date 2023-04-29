module Pol_Integral_de_un_polinomio_Spec (main, spec) where

import Pol_Integral_de_un_polinomio
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

ejPol :: (Num a, Eq a) => Polinomio a
ejPol = consPol 7 2 (consPol 4 5 (consPol 2 5 polCero))

spec :: Spec
spec = do
  it "e1" $
    show (integral ejPol) `shouldBe` "0.25*x^8 + x^5 + 1.6666666666666667*x^3"
  it "e2" $
    show (integral ejPol :: Polinomio Rational) `shouldBe` "1 % 4*x^8 + x^5 + 5 % 3*x^3"
