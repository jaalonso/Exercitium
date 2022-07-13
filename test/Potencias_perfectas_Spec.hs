module Potencias_perfectas_Spec (main, spec) where

import Potencias_perfectas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG potenciasPerfectas' = do
  it "e1" $
    take 10 potenciasPerfectas' `shouldBe` [4,8,9,16,25,27,32,36,49,64]

spec :: Spec
spec = do
  describe "def. 1" $ specG potenciasPerfectas1
  describe "def. 2" $ specG potenciasPerfectas2
  describe "def. 3" $ specG potenciasPerfectas3
  describe "equivalencia" $ it "p1" $ property prop_potenciasPerfectas
