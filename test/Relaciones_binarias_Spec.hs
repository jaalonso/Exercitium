module Relaciones_binarias_Spec (main, spec) where

import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG esRelacionBinaria' = do
  it "e1" $
    esRelacionBinaria' (R ([1, 3], [(3, 1), (3, 3)])) `shouldBe` True
  it "e2" $
    esRelacionBinaria' (R ([1, 3], [(3, 1), (3, 2)])) `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG esRelacionBinaria
  describe "def. 2" $ specG esRelacionBinaria2
  describe "def. 3" $ specG esRelacionBinaria3
  describe "equivalencia" $ it "p1" $ property prop_esRelacionBinaria
