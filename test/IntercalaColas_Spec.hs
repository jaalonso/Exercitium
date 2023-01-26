module IntercalaColas_Spec (main, spec) where

import IntercalaColas
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Cola Int -> Cola Int) -> Spec
specG intercalaColas' = do
  it "e1" $
    show (intercalaColas' ej1 ej2) `shouldBe` "5 | 9 | 3 | 4 | 7 | 0"
  it "e2" $
    show (intercalaColas' ej2 ej1) `shouldBe` "9 | 5 | 4 | 3 | 7 | 0"
  where
    ej1 = inserta 3 (inserta 5 vacia)
    ej2 = inserta 0 (inserta 7 (inserta 4 (inserta 9 vacia)))

spec :: Spec
spec = do
  describe "def. 1" $ specG intercalaColas
  describe "def. 2" $ specG intercalaColas2
  describe "def. 3" $ specG intercalaColas3
  describe "equivalencia" $ it "p1" $ property prop_intercalaColas
