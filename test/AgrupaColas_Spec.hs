module AgrupaColas_Spec (main, spec) where

import AgrupaColas
import TAD.Cola
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Cola Int] -> Cola Int) -> Spec
specG agrupaColas = do
  it "e1" $
    show (agrupaColas [ej1]) `shouldBe` "5 | 2"
  it "e2" $
    show (agrupaColas [ej1, ej2]) `shouldBe` "5 | 4 | 2 | 7 | 3"
  it "e3" $
    show (agrupaColas [ej1, ej2, ej3]) `shouldBe` "5 | 6 | 4 | 1 | 2 | 0 | 7 | 9 | 3"
  where
    ej1 = inserta 2 (inserta 5 vacia)
    ej2 = inserta 3 (inserta 7 (inserta 4 vacia))
    ej3 = inserta 9 (inserta 0 (inserta 1 (inserta 6 vacia)))

spec :: Spec
spec = do
  describe "def. 1" $ specG agrupaColas1
  describe "def. 2" $ specG agrupaColas2
