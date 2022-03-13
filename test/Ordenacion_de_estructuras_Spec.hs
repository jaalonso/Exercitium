module Ordenacion_de_estructuras_Spec (main, spec) where

import Ordenacion_de_estructuras
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Notas] -> [Notas]) -> Spec
specG ordenadas = do
  it "e1" $
    ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7]
    `shouldBe` [Notas "Juan" 6 5,Notas "Luis" 3 7]
  it "e2" $
    ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4]
    `shouldBe` [Notas "Luis" 3 4,Notas "Juan" 6 5]
  it "e3" $
    ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4]
    `shouldBe` [Notas "Luis" 7 4,Notas "Juan" 6 5]
  it "e4" $
    ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4]
    `shouldBe` [Notas "Juan" 6 4,Notas "Luis" 7 4]
  it "e5" $
    ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4]
    `shouldBe` [Notas "Luis" 5 4,Notas "Juan" 6 4]
  it "e6" $
    ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4]
    `shouldBe` [Notas "Juan" 5 4,Notas "Luis" 5 4]
  it "e7" $
    ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4]
    `shouldBe` [Notas "Eva" 5 4,Notas "Juan" 5 4]

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadas1
  describe "def. 2" $ specG ordenadas2
  describe "def. 3" $ specG ordenadas3
  describe "def. 4" $ specG ordenadas4
  describe "equivalencia" $ it "p1" $ property prop_ordenadas
