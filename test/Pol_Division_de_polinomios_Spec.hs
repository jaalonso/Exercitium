module Pol_Division_de_polinomios_Spec (main, spec) where

import Pol_Division_de_polinomios
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (cociente pol1 pol2) `shouldBe` "2.0*x + 3.0"
  it "e2" $
    show (resto pol1 pol2) `shouldBe` "1.0*x + 4.0"
  where
    pol1 = consPol 3 2 (consPol 2 9 (consPol 1 10 (consPol 0 4 polCero)))
    pol2 = consPol 2 1 (consPol 1 3 polCero)
