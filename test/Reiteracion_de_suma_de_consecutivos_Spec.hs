module Reiteracion_de_suma_de_consecutivos_Spec (main, spec) where

import Reiteracion_de_suma_de_consecutivos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer] -> Integer) -> Spec
specG sumaReiterada = do
  it "e1" $
    sumaReiterada [1..5]     `shouldBe` (48)
  it "e2" $
    sumaReiterada [5,4..1]   `shouldBe` (48)
  it "e3" $
    sumaReiterada [-1,-1,-1] `shouldBe` (-4)
  it "e4" $
    sumaReiterada [1,2,3,4]  `shouldBe` (20)


spec :: Spec
spec = do
  describe "def. 1" $ specG sumaReiterada1
  describe "def. 2" $ specG sumaReiterada2
  describe "def. 3" $ specG sumaReiterada3
  describe "def. 4" $ specG sumaReiterada4
  describe "def. 5" $ specG sumaReiterada5
  describe "def. 6" $ specG sumaReiterada6
  describe "def. 7" $ specG sumaReiterada7
  describe "def. 8" $ specG sumaReiterada8
  describe "def. 9" $ specG sumaReiterada9
  describe "def. 10" $ specG sumaReiterada10
