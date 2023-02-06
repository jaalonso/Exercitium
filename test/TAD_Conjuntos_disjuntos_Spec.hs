module TAD_Conjuntos_disjuntos_Spec (main, spec) where

import TAD_Conjuntos_disjuntos
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Bool) -> Spec
specG disjuntos' = do
  it "e1" $
    disjuntos' ej1 ej2 `shouldBe` True
  it "e2" $
    disjuntos' ej1 ej3 `shouldBe` False
  where
    ej1 = inserta 2 (inserta 5 vacio)
    ej2 = inserta 4 (inserta 3 vacio)
    ej3 = inserta 5 (inserta 3 vacio)


spec :: Spec
spec = do
  describe "def. 1" $ specG disjuntos
  describe "def. 2" $ specG disjuntos2
  describe "def. 3" $ specG disjuntos3
  describe "equivalencia" $ it "p1" $ property prop_disjuntos
