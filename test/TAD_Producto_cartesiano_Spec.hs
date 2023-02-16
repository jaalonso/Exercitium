module TAD_Producto_cartesiano_Spec (main, spec) where

import TAD_Producto_cartesiano
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj Int -> Conj (Int, Int)) -> Spec
specG productoC' = do
  it "e1" $
    show (productoC' ej1 ej2)
    `shouldBe` "{(2,3), (2,4), (2,9), (5,3), (5,4), (5,9)}"
  where
    ej1 = inserta 2 (inserta 5 vacio)
    ej2 = inserta 9 (inserta 4 (inserta 3 vacio))

spec :: Spec
spec = do
  describe "def. 1" $ specG productoC
  describe "def. 2" $ specG productoC2
  describe "def. 3" $ specG productoC3
  describe "equivalencia" $ it "p1" $ property prop_productoC
