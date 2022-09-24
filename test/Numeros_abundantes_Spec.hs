module Numeros_abundantes_Spec (main, spec) where

import Numeros_abundantes
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Bool) -> Spec
specG numeroAbundante = do
  it "e1" $
    numeroAbundante 5  `shouldBe` False
  it "e2" $
    numeroAbundante 12 `shouldBe` True
  it "e3" $
    numeroAbundante 28 `shouldBe` False
  it "e4" $
    numeroAbundante 30 `shouldBe` True

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroAbundante1
  describe "def. 2" $ specG numeroAbundante2
  describe "equivalencia" $ it "p1" $ property prop_numeroAbundante
