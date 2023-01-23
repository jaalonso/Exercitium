module LongitudCola_Spec (main, spec) where

import LongitudCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Int) -> Spec
specG longitudCola =
  it "e1" $
    longitudCola (inserta 4 (inserta 2 (inserta 5 vacia))) `shouldBe` 3

spec :: Spec
spec = do
  describe "def. 1" $ specG longitudCola1
  describe "def. 2" $ specG longitudCola2
  describe "equivalencia" $ it "p1" $ property prop_longitudCola
