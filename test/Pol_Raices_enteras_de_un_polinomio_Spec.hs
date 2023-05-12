module Pol_Raices_enteras_de_un_polinomio_Spec (main, spec) where

import Pol_Raices_enteras_de_un_polinomio
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    raicesRuffini ejPol1 `shouldBe` []
  it "e2" $
    raicesRuffini ejPol2 `shouldBe` [0,-1]
  it "e3" $
    raicesRuffini ejPol3 `shouldBe` [0]
  it "e4" $
    raicesRuffini ejPol4 `shouldBe` [1,-1,-2]
  where
    ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
    ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
    ejPol3 = consPol 4 6 (consPol 1 2 polCero)
    ejPol4 = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
