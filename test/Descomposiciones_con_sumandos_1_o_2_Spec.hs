module Descomposiciones_con_sumandos_1_o_2_Spec (main, spec) where

import Descomposiciones_con_sumandos_1_o_2
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: (Int -> [[Int]]) -> Spec
specG1 sumas' = do
  it "e1" $
    sumas' 1 `shouldBe`  [[1]]
  it "e2" $
    sumas' 2 `shouldBe`  [[1,1],[2]]
  it "e3" $
    sumas' 3 `shouldBe`  [[1,1,1],[1,2],[2,1]]
  it "e4" $
    sumas' 4 `shouldBe`  [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[2,2]]
  it "e5" $
    sumas' 5 `shouldBe`
    [[1,1,1,1,1],[1,1,1,2],[1,1,2,1],[1,2,1,1],[1,2,2],[2,1,1,1],[2,1,2],[2,2,1]]

specG2 :: (Int -> Integer) -> Spec
specG2 nSumas' = do
  it "e6" $
    map nSumas' [1..10] `shouldBe` [1,2,3,5,8,13,21,34,55,89]

spec :: Spec
spec = do
  describe "def. 1" $ specG1 sumas1
  describe "def. 2" $ specG1 sumas2
  describe "def. 3" $ specG1 sumas3
  describe "def. 4" $ specG1 sumas4
  describe "equivalencia" $ modifyMaxSize (const 7) $ it "p1" $ property prop_sumas
  describe "def. 5" $ specG2 nSumas1
  describe "def. 6" $ specG2 nSumas2
  describe "def. 7" $ specG2 nSumas3
  describe "def. 8" $ specG2 nSumas4
  describe "equivalencia" $ modifyMaxSize (const 7) $ it "p2" $ property prop_nSumas
