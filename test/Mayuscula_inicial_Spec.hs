module Mayuscula_inicial_Spec (main, spec) where

import Mayuscula_inicial
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (String -> String) -> Spec
specG mayusculaInicial = do
  it "e1" $
    mayusculaInicial "sEviLLa"  `shouldBe`  "Sevilla"
  it "e2" $
    mayusculaInicial ""         `shouldBe`  ""

spec :: Spec
spec = do
  describe "def. 1" $ specG mayusculaInicial1
  describe "def. 2" $ specG mayusculaInicial2
  describe "def. 3" $ specG mayusculaInicial3
  describe "equivalencia" $ it "p1" $ property prop_mayusculaInicial
