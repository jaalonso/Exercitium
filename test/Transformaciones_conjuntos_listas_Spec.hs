module Transformaciones_conjuntos_listas_Spec (main, spec) where

import Transformaciones_conjuntos_listas
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG1 :: ([Int] -> Conj Int) -> Spec
specG1 listaAconjunto' =
  it "e1" $
    show (listaAconjunto' [3, 2, 5]) `shouldBe` "{2, 3, 5}"

specG2 :: (Conj Int -> [Int]) -> Spec
specG2 conjuntoAlista' =
  it "e1" $
    conjuntoAlista' (inserta 5 (inserta 2 (inserta 3 vacio)))
    `shouldBe` [2, 3, 5]

spec :: Spec
spec = do
  describe "def. listaAconjunto 1" $ specG1 listaAconjunto
  describe "def. listaAconjunto 2" $ specG1 listaAconjunto2
  describe "def. conjuntoAlista 1" $ specG2 conjuntoAlista
