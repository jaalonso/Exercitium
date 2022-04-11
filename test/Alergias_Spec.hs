module Alergias_Spec (main, spec) where

import Alergias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> [Alergeno]) -> Spec
specG alergias = do
  it "e1" $
    alergias 1
    `shouldBe` [Huevos]
  it "e2" $
    alergias 2
    `shouldBe` [Cacahuetes]
  it "e3" $
    alergias 3
    `shouldBe` [Huevos,Cacahuetes]
  it "e4" $
    alergias 5
    `shouldBe` [Huevos,Mariscos]
  it "e5" $
    alergias 255
    `shouldBe` [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]

spec :: Spec
spec = do
  describe "def. 1" $ specG alergias1
  describe "def. 2" $ specG alergias2
  describe "def. 3" $ specG alergias3
  describe "def. 4" $ specG alergias4
  describe "def. 5" $ specG alergias5
  describe "equivalencia" $ it "p1" $ property prop_alergias
