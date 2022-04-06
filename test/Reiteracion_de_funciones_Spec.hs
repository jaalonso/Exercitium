module Reiteracion_de_funciones_Spec (main, spec) where

import Reiteracion_de_funciones
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ((Int -> Int) -> Int -> Int -> Int) -> Spec
specG reiteracion = do
  it "e1" $
    reiteracion (+1) 10 5  `shouldBe`  15
  it "e2" $
    reiteracion (+5) 10 0  `shouldBe`  50
  it "e3" $
    reiteracion (*2)  4 1  `shouldBe`  16

spec :: Spec
spec = do
  describe "def. 1" $ specG reiteracion1
  describe "def. 2" $ specG reiteracion2
  describe "def. 3" $ specG reiteracion3
  describe "def. 4" $ specG reiteracion4
  describe "def. 5" $ specG reiteracion5
  describe "def. 6" $ specG reiteracion6
  describe "equivalencia" $ it "p1" $ property prop_reiteracion
