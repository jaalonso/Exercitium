module Pol_Valor_de_un_polinomio_en_un_punto_Spec (main, spec) where

import Pol_Valor_de_un_polinomio_en_un_punto
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    valor ejPol 0 `shouldBe` 3
  it "e2" $
    valor ejPol 1 `shouldBe` 1
  it "e3" $
    valor ejPol (-2) `shouldBe` 31
  where ejPol = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
