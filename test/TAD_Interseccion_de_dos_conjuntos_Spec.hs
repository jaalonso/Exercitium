module TAD_Interseccion_de_dos_conjuntos_Spec (main, spec) where

import TAD_Interseccion_de_dos_conjuntos
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Conj Int) -> Spec
specG interseccion' = do
  it "e1" $
    show (interseccion' ej1 ej2) `shouldBe` "{2, 3}"
  where
    ej1 = inserta 3 (inserta 5 (inserta 2 vacio))
    ej2 = inserta 2 (inserta 4 (inserta 3 vacio))

spec :: Spec
spec = do
  describe "def. 1" $ specG interseccion
  describe "def. 2" $ specG interseccion2
  describe "def. 3" $ specG interseccion3
  describe "equivalencia" $ it "p1" $ property prop_interseccion
