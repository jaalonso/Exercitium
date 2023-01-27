module SubCola_Spec (main, spec) where

import SubCola
import TAD.Cola
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Cola Int -> Bool) -> Spec
specG subCola = do
  it "e1" $
    subCola ej1 ej2 `shouldBe` True
  it "e2" $
    subCola ej1 ej3 `shouldBe` False
  where
    ej1 = inserta 2 (inserta 3 vacia)
    ej2 = inserta 7 (inserta 2 (inserta 3 (inserta 5 vacia)))
    ej3 = inserta 2 (inserta 7 (inserta 3 (inserta 5 vacia)))

spec :: Spec
spec = do
  describe "def. 1" $ specG subCola1
  describe "def. 2" $ specG subCola2
  describe "equivalencia" $ it "p1" $ property prop_subCola
