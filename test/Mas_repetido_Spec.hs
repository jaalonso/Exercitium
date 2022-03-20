module Mas_repetido_Spec (main, spec) where

import Mas_repetido
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> (Int,Int)) -> Spec
specG masRepetido = do
  it "e1" $
    masRepetido [1,1,4,4,1]  `shouldBe`  (4,2)
  it "e2" $
    masRepetido [4,4,1,1,5]  `shouldBe`  (4,2)

spec :: Spec
spec = do
  describe "def. 1" $ specG masRepetido1
  describe "def. 2" $ specG masRepetido2
  describe "def. 3" $ specG masRepetido3
  describe "def. 4" $ specG masRepetido4
  describe "def. 5" $ specG masRepetido5
  describe "def. 6" $ specG masRepetido6
