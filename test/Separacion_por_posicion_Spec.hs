module Separacion_por_posicion_Spec (main, spec) where

import Separacion_por_posicion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> ([Int],[Int])) -> Spec
specG particion = do
  it "e1" $
    particion [3,5,6,2]    `shouldBe`  ([3,6],[5,2])
  it "e2" $
    particion [3,5,6,2,7]  `shouldBe`  ([3,6,7],[5,2])

spec :: Spec
spec = do
  describe "def. 1" $ specG particion1
  describe "def. 2" $ specG particion2
  describe "def. 3" $ specG particion3
  describe "def. 4" $ specG particion4
  describe "def. 5" $ specG particion5
  describe "def. 6" $ specG particion6
  describe "def. 7" $ specG particion7
  describe "def. 8" $ specG particion8
  describe "equivalencia" $ it "p1" $ property prop_particion
