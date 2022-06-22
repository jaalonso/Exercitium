module Union_e_interseccion_general_Spec (main, spec) where

import Union_e_interseccion_general
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: ([[Int]] -> [Int]) -> Spec
specG1 unionGeneral = do
  it "e1" $
    unionGeneral []                    `shouldBe`  []
  it "e2" $
    unionGeneral [[1]]                 `shouldBe`  [1]
  it "e3" $
    unionGeneral [[1],[1,2],[2,3]]     `shouldBe`  [1,2,3]

specG2 :: ([[Int]] -> [Int]) -> Spec
specG2 interseccionGeneral = do
  it "e5" $
    interseccionGeneral [[1]]                      `shouldBe`  [1]
  it "e6" $
    interseccionGeneral [[2],[1,2],[2,3]]          `shouldBe`  [2]
  it "e7" $
    interseccionGeneral [[2,7,5],[1,5,2],[5,2,3]]  `shouldBe`  [2,5]

spec :: Spec
spec = do
  describe "def. 1" $ specG1 unionGeneral1
  describe "def. 2" $ specG1 unionGeneral2
  describe "def. 3" $ specG1 unionGeneral3
  describe "equivalencia" $ it "p1" $ property prop_unionGeneral
  describe "def. 1" $ specG2 interseccionGeneral1
  describe "def. 2" $ specG2 interseccionGeneral2
  describe "def. 3" $ specG2 interseccionGeneral3
  describe "equivalencia" $ it "p1" $ property prop_interseccionGeneral
