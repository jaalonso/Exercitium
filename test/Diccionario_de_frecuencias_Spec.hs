module Diccionario_de_frecuencias_Spec (main, spec) where

import Diccionario_de_frecuencias
import Data.Map
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> Map Int Int) -> Spec
specG frecuencias = do
  it "e1" $
    frecuencias [7,9,7,3,9,7,3,9,9] `shouldBe` fromList [(3,2),(7,3),(9,4)]

spec :: Spec
spec = do
  describe "def. 1" $ specG frecuencias1
  describe "def. 2" $ specG frecuencias2
  describe "def. 3" $ specG frecuencias3
  describe "equivalencia" $ it "p1" $ property prop_frecuencias
