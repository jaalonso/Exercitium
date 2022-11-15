module Elementos_consecutivos_relacionados_Spec (main, spec) where

import Elementos_consecutivos_relacionados
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Int -> Bool) -> [Int] -> Bool) -> Spec
specG relacionados = do
  it "e1" $
    relacionados (<) [2,3,7,9] `shouldBe`  True
  it "e2" $
    relacionados (<) [2,3,1,9] `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG relacionados1
  describe "def. 2" $ specG relacionados2
  describe "def. 3" $ specG relacionados3
  describe "def. 4" $ specG relacionados4
