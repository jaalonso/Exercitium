module Numeracion_de_ternas_Spec (main, spec) where

import Numeracion_de_ternas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int,Int,Int) -> Int) -> Spec
specG posicion' = do
  it "e1" $
      posicion' (0,1,0)  `shouldBe`  2
  it "e2" $
      posicion' (0,0,2)  `shouldBe`  4
  it "e3" $
      posicion' (0,1,1)  `shouldBe`  5

spec :: Spec
spec = do
  describe "def. 1" $ specG posicion1
  describe "def. 2" $ specG posicion2
  describe "def. 3" $ specG posicion3
  describe "def. 4" $ specG posicion4
  describe "def. 5" $ specG posicion5
