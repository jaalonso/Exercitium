module Primos_con_cubos_Spec (main, spec) where

import Primos_con_cubos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG primosConCubos = do
  it "e1" $
    take 6 primosConCubos `shouldBe` [7,19,37,61,127,271]

spec :: Spec
spec = do
  describe "def. 1" $ specG primosConCubos1
  describe "def. 2" $ specG primosConCubos2
  describe "def. 3" $ specG primosConCubos3
  describe "def. 4" $ specG primosConCubos4
