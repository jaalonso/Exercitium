module MaxPila_Spec (main, spec) where

import MaxPila
import TAD.Pila
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Int) -> Spec
specG maxPila =
  it "e1" $
    maxPila (apila 3 (apila 5 (apila 1 vacia))) `shouldBe` 5

spec :: Spec
spec = do
  describe "def. 1" $ specG maxPila1
  describe "def. 2" $ specG maxPila2
  describe "equivalencia" $ it "p1" $ property prop_maxPila
