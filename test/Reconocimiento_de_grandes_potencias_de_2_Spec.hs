module Reconocimiento_de_grandes_potencias_de_2_Spec (main, spec) where

import Reconocimiento_de_grandes_potencias_de_2
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Bool) -> Spec
specG esPotenciaDeDos = do
  it "e1" $
    esPotenciaDeDos    2 `shouldBe` True
  it "6" $ do
    esPotenciaDeDos    6 `shouldBe` False
  it "8" $ do
    esPotenciaDeDos    8 `shouldBe` True
  it "1024" $ do
    esPotenciaDeDos 1024 `shouldBe` True
  it "1026" $ do
    esPotenciaDeDos 1026 `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG esPotenciaDeDos1
  describe "def. 2" $ specG esPotenciaDeDos2
  describe "def. 3" $ specG esPotenciaDeDos3
  describe "def. 4" $ specG esPotenciaDeDos4
  describe "equivalencia" $ it "p1" $ property prop_esPotenciaDeDos
