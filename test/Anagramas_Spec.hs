module Anagramas_Spec (main, spec) where

import Anagramas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (String -> [String] -> [String]) -> Spec
specG anagramas' = do
  it "e1" $
    anagramas' "amor" ["Roma","mola","loma","moRa", "rama"] `shouldBe`
    ["Roma","moRa"]
  it "e2" $
    anagramas' "rama" ["aMar","amaRa","roMa","marr","aRma"] `shouldBe`
    ["aMar","aRma"]

spec :: Spec
spec = do
  describe "def. 1" $ specG anagramas
  describe "def. 2" $ specG anagramas2
  describe "def. 3" $ specG anagramas3
  describe "def. 4" $ specG anagramas4
  describe "def. 5" $ specG anagramas5
  describe "def. 6" $ specG anagramas6
  describe "def. 7" $ specG anagramas7
