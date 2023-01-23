module UltimoCola_Spec (main, spec) where

import UltimoCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Int) -> Spec
specG ultimoCola' = do
  it "e1" $
    ultimoCola' (inserta 3 (inserta 5 (inserta 2 vacia))) `shouldBe` 3
  it "e2" $
    ultimoCola' (inserta 2 vacia)                         `shouldBe` 2

spec :: Spec
spec = do
  describe "def. 1" $ specG ultimoCola
  describe "def. 2" $ specG ultimoCola2
  describe "equivalencia" $ it "p1" $ property prop_ultimoCola
