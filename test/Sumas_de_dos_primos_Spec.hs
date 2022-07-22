module Sumas_de_dos_primos_Spec (main, spec) where

import Sumas_de_dos_primos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG sumasDeDosPrimos = 
  it "e1" $
    take 23 sumasDeDosPrimos `shouldBe`
    [4,5,6,7,8,9,10,12,13,14,15,16,18,19,20,21,22,24,25,26,28,30,31]
  
spec :: Spec
spec = do
  describe "def. 1" $ specG sumasDeDosPrimos1
  describe "def. 2" $ specG sumasDeDosPrimos2
  describe "def. 3" $ specG sumasDeDosPrimos3
  describe "def. 4" $ specG sumasDeDosPrimos4
  describe "equivalencia" $ it "p1" $ property prop_sumasDeDosPrimos
