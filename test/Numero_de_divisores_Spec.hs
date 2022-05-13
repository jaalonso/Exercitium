module Numero_de_divisores_Spec (main, spec) where

import Numero_de_divisores
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG numeroDivisores = do
  it "e1" $
    numeroDivisores 12  `shouldBe`  6
  it "e1" $
    numeroDivisores 25  `shouldBe`  3

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroDivisores1
  describe "def. 2" $ specG numeroDivisores2
  describe "def. 3" $ specG numeroDivisores3
  describe "def. 4" $ specG numeroDivisores4
  describe "def. 5" $ specG numeroDivisores5
  describe "equivalencia" $ it "p1" $ property prop_numeroDivisores
