module OrdenadaCola_Spec (main, spec) where

import OrdenadaCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Bool) -> Spec
specG ordenadaCola' = do
  it "e1" $
    ordenadaCola' (inserta 6 (inserta 5 (inserta 1 vacia))) `shouldBe` True
  it "e2" $
    ordenadaCola' (inserta 1 (inserta 0 (inserta 6 vacia))) `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadaCola
  describe "def. 2" $ specG ordenadaCola2
  describe "equivalencia" $ it "p1" $ property prop_ordenadaCola
