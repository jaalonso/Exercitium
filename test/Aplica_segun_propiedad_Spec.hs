module Aplica_segun_propiedad_Spec (main, spec) where

import Aplica_segun_propiedad
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]) -> Spec
specG filtraAplica =
  it "e1" $
    filtraAplica (4+) (<3) [1..7]  `shouldBe`  [5,6]

spec :: Spec
spec = do
  describe "def. 1" $ specG filtraAplica1
  describe "def. 2" $ specG filtraAplica2
  describe "def. 3" $ specG filtraAplica3
  describe "def. 4" $ specG filtraAplica4
  describe "def. 5" $ specG filtraAplica5
