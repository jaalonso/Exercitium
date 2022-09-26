module Numeros_abundantes_impares_Spec (main, spec) where

import Numeros_abundantes_impares
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG abundantesImpares = do
  it "e1" $
    head abundantesImpares == 945

spec :: Spec
spec = do
  describe "def. 1" $ specG abundantesImpares1
  describe "def. 2" $ specG abundantesImpares2
  describe "def. 3" $ specG abundantesImpares3
