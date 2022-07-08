module Clausura_transitiva_de_una_relacion_binaria_Spec (main, spec) where

import Clausura_transitiva_de_una_relacion_binaria
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([(Int,Int)] -> [(Int,Int)]) -> Spec
specG clausuraTransitiva = do
  it "e1" $
    sort (clausuraTransitiva [(1,2),(2,5),(5,6)]) `shouldBe`
    [(1,2),(1,5),(1,6),(2,5),(2,6),(5,6)]
  it "e2" $
    sort (clausuraTransitiva [(1,2),(2,5),(5,6),(6,3)]) `shouldBe`
    [(1,2),(1,3),(1,5),(1,6),(2,3),(2,5),(2,6),(5,3),(5,6),(6,3)]

spec :: Spec
spec = do
  describe "def. 1" $ specG clausuraTransitiva1
  describe "def. 2" $ specG clausuraTransitiva2
  describe "def. 3" $ specG clausuraTransitiva3
  describe "equivalencia" $ it "p1" $ property prop_clausuraTransitiva
