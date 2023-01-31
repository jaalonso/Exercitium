module TAD_subconjunto_Spec (main, spec) where

import TAD_subconjunto
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Bool) -> Spec
specG subconjunto' = do
  it "e1" $
    subconjunto' ej1 ej2 `shouldBe` True
  it "e2" $
    subconjunto' ej1 ej3 `shouldBe` False
  where
    ej1 = inserta 5 (inserta 2 vacio)
    ej2 = inserta 3 (inserta 2 (inserta 5 vacio))
    ej3 = inserta 3 (inserta 4 (inserta 5 vacio))

spec :: Spec
spec = do
  describe "def. 1" $ specG subconjunto
  describe "def. 2" $ specG subconjunto2
  describe "def. 3" $ specG subconjunto3
  describe "def. 4" $ specG subconjunto4
  describe "equivalencia" $ it "p1" $ property prop_subconjunto
