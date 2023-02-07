module TAD_Diferencia_simetrica_Spec (main, spec) where

import TAD_Diferencia_simetrica
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Conj Int) -> Spec
specG diferenciaSimetrica' =
  it "e1" $
    show (diferenciaSimetrica' ej1 ej2) `shouldBe` "{2, 4, 5}"
  where
    ej1 = inserta 5 (inserta 3 (inserta 2 (inserta 7 vacio)))
    ej2 = inserta 7 (inserta 4 (inserta 3 vacio))

spec :: Spec
spec = do
  describe "def. 1" $ specG diferenciaSimetrica
  describe "def. 2" $ specG diferenciaSimetrica2
  describe "equivalencia" $ it "p1" $ property prop_diferenciaSimetrica
