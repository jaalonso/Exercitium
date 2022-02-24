module Ternas_pitagoricas_con_suma_dada_Spec (main, spec) where

import Ternas_pitagoricas_con_suma_dada
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> [(Integer,Integer,Integer)]) -> Spec
specG ternasPitagoricas = do
  it "e1" $
    ternasPitagoricas 12      `shouldBe` [(3,4,5)]
  it "e2" $
    ternasPitagoricas 60      `shouldBe` [(10,24,26),(15,20,25)]

spec :: Spec
spec = do
  describe "def. 1" $ specG ternasPitagoricas1
  describe "def. 2" $ specG ternasPitagoricas2
  describe "def. 3" $ specG ternasPitagoricas3
