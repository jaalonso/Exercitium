module TAD_Diferencia_de_conjuntos_Spec (main, spec) where

import TAD_Diferencia_de_conjuntos
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Conj Int) -> Spec
specG diferencia' = do
  it "e1" $
    show (diferencia' ej1 ej2) `shouldBe` "{2, 5}"
  it "e2" $
    show (diferencia' ej2 ej1) `shouldBe` "{4}"
  it "e3" $
    show (diferencia' ej1 ej1) `shouldBe` "{}"
  where
    ej1 = inserta 5 (inserta 3 (inserta 2 (inserta 7 vacio)))
    ej2 = inserta 7 (inserta 4 (inserta 3 vacio))

spec :: Spec
spec = do
  describe "def. 1" $ specG diferencia
  describe "def. 2" $ specG diferencia2
  describe "equivalencia" $ it "p1" $ property prop_diferencia
