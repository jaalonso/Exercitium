module Ceros_finales_del_factorial_Spec (main, spec) where

import Ceros_finales_del_factorial
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG cerosDelFactorial = do
  it "e1" $
    cerosDelFactorial 24 `shouldBe`  4 
  it "e2" $
    cerosDelFactorial 25 `shouldBe`  6

spec :: Spec
spec = do
  describe "def. 1" $ specG cerosDelFactorial1
  describe "def. 2" $ specG cerosDelFactorial2
  describe "def. 3" $ specG cerosDelFactorial3
  describe "equivalencia" $ it "p1" $ property prop_cerosDelFactorial
