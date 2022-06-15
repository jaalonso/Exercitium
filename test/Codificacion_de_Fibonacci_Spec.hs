module Codificacion_de_Fibonacci_Spec (main, spec) where

import Codificacion_de_Fibonacci
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> String) -> Spec
specG codigoFib' = do
  it "e1" $
    codigoFib' 65
    `shouldBe` "0100100011"
  it "e2" $
    [codigoFib' n | n <- [1..7]]
    `shouldBe` ["11","011","0011","1011","00011","10011","01011"]


spec :: Spec
spec = do
  describe "def. 1" $ specG codigoFib1
  describe "def. 2" $ specG codigoFib2
  describe "equivalencia" $ it "p1" $ property prop_codigoFib
