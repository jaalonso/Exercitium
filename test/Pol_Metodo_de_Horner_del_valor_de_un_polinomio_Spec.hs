module Pol_Metodo_de_Horner_del_valor_de_un_polinomio_Spec (main, spec) where

import Pol_Metodo_de_Horner_del_valor_de_un_polinomio
import TAD.Polinomio
import Data.Ratio
import Test.Hspec

main :: IO ()
main = hspec spec

pol1 :: (Num a, Eq a) => Polinomio a
pol1 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))

spec :: Spec
spec = do
  it "e1" $
    horner pol1 0 `shouldBe` 0
  it "e2" $
    horner pol1 1 `shouldBe` 10
  it "e3" $
    horner pol1 1.5 `shouldBe` 24.84375
  it "e4" $
    horner pol1 (3%2) `shouldBe` 795 % 32
