module Numeros_autodescriptivos_Spec (main, spec) where

import Numeros_autodescriptivos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Bool) -> Spec
specG autodescriptivo = do
  it "e1" $
    [x | x <- [1..22000], autodescriptivo x]
    `shouldBe`  [1210,2020,21200]

spec :: Spec
spec = do
  describe "def. 1" $ specG autodescriptivo1
  describe "def. 2" $ specG autodescriptivo2
  describe "equivalencia" $ it "p1" $ property prop_autodescriptivo
