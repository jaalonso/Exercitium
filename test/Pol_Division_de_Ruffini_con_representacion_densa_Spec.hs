module Pol_Division_de_Ruffini_con_representacion_densa_Spec (main, spec) where

import Pol_Division_de_Ruffini_con_representacion_densa
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> [Int] -> [Int]) -> Spec
specG ruffiniDensa' = do
  it "e1" $
    ruffiniDensa' 2 [1,2,-1,-2] `shouldBe` [1,4,7,12]
  it "e2" $
    ruffiniDensa' 1 [1,2,-1,-2] `shouldBe` [1,3,2,0]

spec :: Spec
spec = do
  describe "def. 1" $ specG ruffiniDensa
  describe "def. 2" $ specG ruffiniDensa2
  describe "equivalencia" $ it "p1" $ property prop_ruffiniDensa
