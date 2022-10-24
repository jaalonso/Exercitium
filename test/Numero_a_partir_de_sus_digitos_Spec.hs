module Numero_a_partir_de_sus_digitos_Spec (main, spec) where

import Numero_a_partir_de_sus_digitos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer] -> Integer) -> Spec
specG listaNumero = do
  it "e1" $
    listaNumero [5]        `shouldBe` 5
  it "e2" $
    listaNumero [1,3,4,7]  `shouldBe` 1347
  it "e3" $
    listaNumero [0,0,1]    `shouldBe` 1

spec :: Spec
spec = do
  describe "def. 1" $ specG listaNumero1
  describe "def. 2" $ specG listaNumero2
  describe "def. 3" $ specG listaNumero3
  describe "def. 4" $ specG listaNumero4
  describe "def. 5" $ specG listaNumero5
  describe "def. 6" $ specG listaNumero6
  describe "def. 7" $ specG listaNumero7
  describe "equivalencia" $ it "p1" $ property prop_listaNumero
