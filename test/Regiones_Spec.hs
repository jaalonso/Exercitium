module Regiones_Spec (main, spec) where

import Regiones
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG regiones = do
  it "e1" $
    regiones 1     `shouldBe`  2
  it "e2" $
    regiones 2     `shouldBe`  4
  it "e3" $
    regiones 3     `shouldBe`  7
  it "e4" $
    regiones 100   `shouldBe`  5051

spec :: Spec
spec = do
  describe "def. 1" $ specG regiones1
  describe "def. 2" $ specG regiones2
  describe "def. 3" $ specG regiones3
  describe "def. 4" $ specG regiones4
