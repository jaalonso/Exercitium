module Exponente_mayor_Spec (main, spec) where

import Exponente_mayor
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer -> Integer) -> Spec
specG mayorExponente = do
  it "e1" $
    mayorExponente 2 8    `shouldBe`  3
  it "e2" $
    mayorExponente 2 9    `shouldBe`  0
  it "e3" $
    mayorExponente 5 100  `shouldBe`  2
  it "e4" $
    mayorExponente 2 60   `shouldBe`  2

spec :: Spec
spec = do
  describe "def. 1" $ specG mayorExponente1
  describe "def. 2" $ specG mayorExponente2
  describe "def. 3" $ specG mayorExponente3
  describe "def. 4" $ specG mayorExponente4
  describe "equivalencia" $ it "p1" $ property prop_mayorExponente
