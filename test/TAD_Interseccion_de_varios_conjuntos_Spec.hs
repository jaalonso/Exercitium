module TAD_Interseccion_de_varios_conjuntos_Spec (main, spec) where

import TAD_Interseccion_de_varios_conjuntos
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Conj Int] -> Conj Int) -> Spec
specG interseccionG' =
  it "e1" $
    show (interseccionG' [ej1, ej2, ej3]) `shouldBe` "{2, 5}"
  where
    ej1 = inserta 2 (inserta 3 (inserta 5 vacio))
    ej2 = inserta 5 (inserta 2 (inserta 7 vacio))
    ej3 = inserta 3 (inserta 2 (inserta 5 vacio))

spec :: Spec
spec = do
  describe "def. 1" $ specG interseccionG
  describe "def. 2" $ specG interseccionG2
  describe "equivalencia" $ it "p1" $ property prop_interseccionG
