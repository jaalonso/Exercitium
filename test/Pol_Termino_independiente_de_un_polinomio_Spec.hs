module Pol_Termino_independiente_de_un_polinomio_Spec (main, spec) where

import Pol_Termino_independiente_de_un_polinomio
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    terminoIndep ejPol1 `shouldBe` 3
  it "e2" $
    terminoIndep ejPol2 `shouldBe` 0
  where
    ejPol1 = consPol 4 3 (consPol 2 5 (consPol 0 3 polCero))
    ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
