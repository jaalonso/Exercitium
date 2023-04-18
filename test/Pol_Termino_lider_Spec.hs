module Pol_Termino_lider_Spec (main, spec) where

import Pol_Termino_lider
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (termLider ejPol) `shouldBe` "x^5"
  where ejPol = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
