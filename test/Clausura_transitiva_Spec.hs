module Clausura_transitiva_Spec (main, spec) where

import Clausura_transitiva
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Rel Int) -> Spec
specG clausuraTransitiva' = do
  it "e1" $
    show (clausuraTransitiva' (R ([1..6],[(1,2),(2,5),(5,6)])))
    `shouldBe` "R ([1,2,3,4,5,6],[(1,2),(2,5),(5,6),(1,5),(2,6),(1,6)])"

spec :: Spec
spec = do
  describe "def. 1" $ specG clausuraTransitiva
  describe "def. 2" $ specG clausuraTransitiva2
  describe "equivalencia" $ it "p1" $ property prop_clausuraTransitiva
