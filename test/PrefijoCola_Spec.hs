module PrefijoCola_Spec (main, spec) where

import PrefijoCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Cola Int -> Bool) -> Spec
specG prefijoCola' = do
  it "e1" $
    prefijoCola' ej1 ej2 `shouldBe` True
  it "e2" $
    prefijoCola' ej1 ej3 `shouldBe` False
  where
    ej1 = inserta 4 (inserta 2 vacia)
    ej2 = inserta 5 (inserta 4 (inserta 2 vacia))
    ej3 = inserta 5 (inserta 2 (inserta 4 vacia))

spec :: Spec
spec = do
  describe "def. 1" $ specG prefijoCola
  describe "def. 2" $ specG prefijoCola2
  describe "equivalencia" $ it "p1" $ property prop_prefijoCola
