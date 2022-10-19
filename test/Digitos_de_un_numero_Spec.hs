module Digitos_de_un_numero_Spec (main, spec) where

import Digitos_de_un_numero
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Int]) -> Spec
specG digitos =
  it "e1" $
    digitos 320274  `shouldBe`  [3,2,0,2,7,4]

spec :: Spec
spec = do
  describe "def. 1" $ specG digitos1
  describe "def. 2" $ specG digitos2
  describe "def. 3" $ specG digitos3
  describe "def. 4" $ specG digitos4
  describe "def. 5" $ specG digitos5
  describe "def. 6" $ specG digitos6
  describe "def. 7" $ specG digitos7
  describe "def. 8" $ specG digitos8
  describe "def. 9" $ specG digitos9
  describe "def. 9" $ specG digitos10
  describe "equivalencia" $ it "p1" $ property prop_digitos
