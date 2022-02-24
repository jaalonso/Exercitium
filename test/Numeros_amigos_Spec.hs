module Numeros_amigos_Spec (main, spec) where

import Numeros_amigos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer -> Bool) -> Spec
specG amigos = do
  it "e1" $
    amigos 220 284 `shouldBe` True
  it "e2" $
    amigos 220 23  `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG amigos1
  describe "def. 2" $ specG amigos2
  describe "def. 3" $ specG amigos3
  describe "def. 4" $ specG amigos4
  describe "def. 5" $ specG amigos5
  describe "def. 6" $ specG amigos6
  describe "def. 7" $ specG amigos7
