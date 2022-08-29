module Union_conjuntista_de_listas_Spec (main, spec) where

import Union_conjuntista_de_listas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG union' = do
  it "e1" $
    union' [3,2,5] [5,7,3,4]  `shouldMatchList`  [3,2,5,7,4]

spec :: Spec
spec = do
  describe "def. 1" $ specG union1
  describe "def. 2" $ specG union2
  describe "def. 3" $ specG union3
  describe "equivalencia" $ it "p1" $ property prop_union
