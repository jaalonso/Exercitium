module Limite_del_seno_Spec (main, spec) where

import Limite_del_seno
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: (Int -> [Double]) -> Spec
specG1 aproxLimSeno =
  it "e1" $
    aproxLimSeno 2 `shouldBe` [0.8414709848078965,0.958851077208406]

specG2 :: (Double -> Int) -> Spec
specG2 errorLimSeno = do
  it "e1" $
    errorLimSeno 0.1     `shouldBe`   2
  it "e2" $
    errorLimSeno 0.01    `shouldBe`   5
  it "e3" $
    errorLimSeno 0.001   `shouldBe`  13
  it "e4" $
    errorLimSeno 0.0001  `shouldBe`  41

spec :: Spec
spec = do
  describe "def. 1" $ specG1 aproxLimSeno1
  describe "def. 2" $ specG1 aproxLimSeno2
  describe "def. 3" $ specG1 aproxLimSeno3
  describe "def. 4" $ specG1 aproxLimSeno4
  describe "def. 5" $ specG1 aproxLimSeno5
  describe "equivalencia" $ it "p1" $ property prop_aproxLimSeno
  describe "def. 1" $ specG2 errorLimSeno1
  describe "def. 2" $ specG2 errorLimSeno2
  describe "def. 3" $ specG2 errorLimSeno3
  describe "equivalencia" $ it "p1" $ property prop_errorLimSeno
