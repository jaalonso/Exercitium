module Exponente_en_la_factorizacion_Spec (main, spec) where

import Exponente_en_la_factorizacion
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer -> Int) -> Spec
specG exponente = do
  it "e1" $
    exponente 2 24 `shouldBe` 3
  it "e2" $
    exponente 2 17280 `shouldBe` 7
  it "e3" $
    exponente 2 22222222222 `shouldBe` 1
  it "e4" $
    exponente 2 256 `shouldBe` 8
  it "e5" $
    exponente 2 1 `shouldBe` 0
  it "e6" $
    exponente 2 2 `shouldBe` 1
  it "e7" $
    exponente 2 256 `shouldBe` 8
  it "e8" $
    exponente 2 482848428248882482 `shouldBe` 1
  it "e9" $
    exponente 2 7 `shouldBe` 0

spec :: Spec
spec = do
  describe "def. 1" $ specG exponente1
  describe "def. 2" $ specG exponente2
  describe "def. 3" $ specG exponente3
