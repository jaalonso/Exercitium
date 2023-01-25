module ExtiendeCola_Spec (main, spec) where

import ExtiendeCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Cola Int -> Cola Int) -> Spec
specG extiendeCola' = do
  it "e1" $
    show (extiendeCola' ej1 ej2) `shouldBe` "2 | 3 | 4 | 3 | 5"
  it "e2" $
    show (extiendeCola' ej2 ej1) `shouldBe` "4 | 3 | 5 | 2 | 3"
  where ej1 = inserta 3 (inserta 2 vacia)
        ej2 = inserta 5 (inserta 3 (inserta 4 vacia))

spec :: Spec
spec = do
  describe "def. 1" $ specG extiendeCola
  describe "def. 2" $ specG extiendeCola2
  describe "equivalencia" $ it "p1" $ property prop_extiendeCola
