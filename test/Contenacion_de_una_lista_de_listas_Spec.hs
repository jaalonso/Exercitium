module Contenacion_de_una_lista_de_listas_Spec (main, spec) where

import Contenacion_de_una_lista_de_listas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([[Int]] -> [Int]) -> Spec
specG conc =
  it "e1" $
    conc [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]

spec :: Spec
spec = do
  describe "def. 1" $ specG conc1
  describe "def. 2" $ specG conc2
  describe "def. 3" $ specG conc3
  describe "def. 4" $ specG conc4
  describe "equivalencia" $ it "p1" $ property prop_conc
