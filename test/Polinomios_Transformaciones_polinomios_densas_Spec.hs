module Polinomios_Transformaciones_polinomios_densas_Spec (main, spec) where

import Polinomios_Transformaciones_polinomios_densas
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (densaApolinomio [9,0,0,5,0,4,7])
    `shouldBe` "9*x^6 + 5*x^3 + 4*x + 7"
  it "e2" $
    show (densaApolinomio2 [9,0,0,5,0,4,7])
    `shouldBe` "9*x^6 + 5*x^3 + 4*x + 7"
  it "e3" $
    polinomioAdensa (consPol 6 9 (consPol 3 5 (consPol 1 4 (consPol 0 7 polCero))))
    `shouldBe` [9,0,0,5,0,4,7]
  it "e4" $
    polinomioAdensa2 (consPol 6 9 (consPol 3 5 (consPol 1 4 (consPol 0 7 polCero))))
    `shouldBe` [9,0,0,5,0,4,7]
