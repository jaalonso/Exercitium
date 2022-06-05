module Codificacion_de_Godel_Spec (main, spec) where

import Codificacion_de_Godel
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: ([Integer] -> Integer) -> Spec
specG1 codificaG' = do
  it "e1" $
    codificaG' [6,0,4]            `shouldBe`  1200000
  it "e2" $
    codificaG' [3,1,1]            `shouldBe`  3600
  it "e3" $
    codificaG' [3,1,0,0,0,0,0,1]  `shouldBe`  4423058640
  it "e4" $
    codificaG' [1..6]             `shouldBe`  126111168580452537982500

specG2 :: (Integer -> [Integer]) -> Spec
specG2 decodificaG' = do
  it "e1" $
    decodificaG' 1200000                   `shouldBe`  [6,0,4]
  it "e2" $
    decodificaG' 3600                      `shouldBe`  [3,1,1]
  it "e3" $
    decodificaG' 4423058640                `shouldBe`  [3,1,0,0,0,0,0,1]
  it "e4" $
    decodificaG' 126111168580452537982500  `shouldBe`  [1,2,3,4,5,6]

spec :: Spec
spec = do
  describe "def. 1" $ specG1 codificaG1
  describe "def. 2" $ specG1 codificaG2
  describe "def. 3" $ specG1 codificaG3
  describe "def. 4" $ specG1 codificaG4
  describe "equivalencia" $ it "p1" $ property prop_codificaG
  describe "def. 5" $ specG2 decodificaG
