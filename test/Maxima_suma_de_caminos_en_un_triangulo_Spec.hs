module Maxima_suma_de_caminos_en_un_triangulo_Spec (main, spec) where

import Maxima_suma_de_caminos_en_un_triangulo
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([[Integer]] -> Integer) -> Spec
specG maximaSuma = do
  it "e1" $
    maximaSuma [[3],[7,4]]                    `shouldBe`  10
  it "e2" $
    maximaSuma [[3],[7,4],[2,4,6]]            `shouldBe`  14
  it "e3" $
    maximaSuma [[3],[7,4],[2,4,6],[8,5,9,3]]  `shouldBe`  23

spec :: Spec
spec = do
  describe "def. 1" $ specG maximaSuma1
  describe "def. 2" $ specG maximaSuma2
  describe "def. 3" $ specG maximaSuma3
  describe "def. 4" $ specG maximaSuma4
  describe "def. 5" $ specG maximaSuma5
  describe "def. 6" $ specG maximaSuma6
  describe "def. 7" $ specG maximaSuma7
  describe "def. 8" $ specG maximaSuma8
