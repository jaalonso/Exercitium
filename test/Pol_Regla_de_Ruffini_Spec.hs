module Pol_Regla_de_Ruffini_Spec (main, spec) where

import Pol_Regla_de_Ruffini
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (cocienteRuffini 2 ejPol) `shouldBe` "x^2 + 4*x + 7"
  it "e2" $
    show (cocienteRuffini (-2) ejPol) `shouldBe` "x^2 + -1"
  it "e3" $
    show (cocienteRuffini 3 ejPol) `shouldBe` "x^2 + 5*x + 14"
  it "e4" $
    restoRuffini 2 ejPol `shouldBe` 12
  it "e5" $
    restoRuffini (-2) ejPol `shouldBe` 0
  it "e6" $
    restoRuffini 3 ejPol `shouldBe` 40
  it "e1'" $
    show (cocienteRuffini2 2 ejPol) `shouldBe` "x^2 + 4*x + 7"
  it "e2'" $
    show (cocienteRuffini2 (-2) ejPol) `shouldBe` "x^2 + -1"
  it "e3'" $
    show (cocienteRuffini2 3 ejPol) `shouldBe` "x^2 + 5*x + 14"
  it "e4'" $
    restoRuffini2 2 ejPol `shouldBe` 12
  it "e5'" $
    restoRuffini2 (-2) ejPol `shouldBe` 0
  it "e6'" $
    restoRuffini2 3 ejPol `shouldBe` 40
  where
    ejPol = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
