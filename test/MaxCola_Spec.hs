module MaxCola_Spec (main, spec) where

import MaxCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Int) -> Spec
specG maxCola =
  it "e1" $
    maxCola (inserta 3 (inserta 5 (inserta 1 vacia))) `shouldBe` 5

spec :: Spec
spec = do
  describe "def. 1" $ specG maxCola1
  describe "def. 2" $ specG maxCola2
  describe "equivalencia" $ it "p1" $ property prop_maxCola
