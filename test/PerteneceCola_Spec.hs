module PerteneceCola_Spec (main, spec) where

import PerteneceCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> Cola Int -> Bool) -> Spec
specG perteneceCola' = do
  it "e1" $
    perteneceCola' 2 (inserta 5 (inserta 2 (inserta 3 vacia))) `shouldBe` True
  it "e2" $
    perteneceCola' 4 (inserta 5 (inserta 2 (inserta 3 vacia))) `shouldBe` False


spec :: Spec
spec = do
  describe "def. 1" $ specG perteneceCola
  describe "def. 2" $ specG perteneceCola2
  describe "equivalencia" $ it "p1" $ property prop_perteneceCola
