module Suma_alterna_de_factoriales_Spec (main, spec) where

import Suma_alterna_de_factoriales
import Test.Hspec

main :: IO ()
main = hspec spec

specG1 :: (Integer -> Integer) -> Spec
specG1 sumaAlterna' = do
  it "e1" $
    sumaAlterna' 3  `shouldBe`  5
  it "e2" $
    sumaAlterna' 4  `shouldBe`  19
  it "e3" $
    sumaAlterna' 5  `shouldBe`  101
  it "e4" $
    sumaAlterna' 6  `shouldBe`  619
  it "e5" $
    sumaAlterna' 7  `shouldBe`  4421
  it "e6" $
    sumaAlterna' 8  `shouldBe`  35899

specG2 :: [Integer] -> Spec
specG2 sumasAlternas = do
  it "e1" $
    take 8 sumasAlternas `shouldBe`
    [0,1,1,5,19,101,619,4421]

specG3 :: [Integer] -> Spec
specG3 conSumaAlternaPrima = do
  it "e1" $
    take 7 conSumaAlternaPrima `shouldBe`
    [3,4,5,6,7,8,10]

spec :: Spec
spec = do
  describe "def. 1" $ specG1 sumaAlterna1
  describe "def. 2" $ specG1 sumaAlterna2
  describe "def. 3" $ specG1 sumaAlterna3
  describe "def. 4" $ specG1 sumaAlterna4
  describe "def. 1" $ specG2 sumasAlternas1
  describe "def. 2" $ specG2 sumasAlternas2
  describe "def. 3" $ specG2 sumasAlternas3
  describe "def. 1" $ specG3 conSumaAlternaPrima1
  describe "def. 2" $ specG3 conSumaAlternaPrima2
  describe "def. 3" $ specG3 conSumaAlternaPrima3
