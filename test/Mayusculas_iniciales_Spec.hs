module Mayusculas_iniciales_Spec (main, spec) where

import Mayusculas_iniciales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([String] -> [String]) -> Spec
specG titulo =
  it "e1" $
    titulo ["eL","arTE","DE","La","proGraMacion"] `shouldBe`
    ["El","Arte","de","la","Programacion"]

spec :: Spec
spec = do
  describe "def. 1" $ specG titulo1
  describe "def. 2" $ specG titulo2
  describe "def. 3" $ specG titulo3
  describe "equivalencia" $ it "p1" $ property prop_titulo
