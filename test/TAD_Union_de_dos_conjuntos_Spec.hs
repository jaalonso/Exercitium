module TAD_Union_de_dos_conjuntos_Spec (main, spec) where

import TAD_Union_de_dos_conjuntos
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Conj Int) -> Spec
specG union' =
  it "e1" $
    show (union' ej1 ej2) `shouldBe` "{3, 4, 5}"
  where
    ej1 = inserta 3 (inserta 5 vacio)
    ej2 = inserta 4 (inserta 3 vacio)

spec :: Spec
spec = do
  describe "def. 1" $ specG union
  describe "def. 2" $ specG union2
  describe "def. 3" $ specG union3
  describe "equivalencia" $ it "p1" $ property prop_union
