module Agrupacion_de_elementos_por_posicion_Spec (main, spec) where

import Agrupacion_de_elementos_por_posicion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([[Int]]-> [[Int]]) -> Spec
specG agrupa =
  it "e1" $
    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]

spec :: Spec
spec = do
  describe "def. 1" $ specG agrupa1
  describe "def. 2" $ specG agrupa2
  describe "def. 3" $ specG agrupa3
  describe "equivalencia" $ it "p1" $ property prop_agrupa
