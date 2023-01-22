module Transformaciones_colas_listas_Spec (main, spec) where

import Transformaciones_colas_listas
import TAD.Cola
import Test.Hspec

main :: IO ()
main = hspec spec

specG1 :: ([Int] -> Cola Int) -> Spec
specG1 listaAcola' =
  it "e1" $
    show (listaAcola' [3, 2, 5]) `shouldBe` "3 | 2 | 5"

specG2 :: (Cola Int -> [Int]) -> Spec
specG2 colaAlista' =
  it "e1" $
    colaAlista' (inserta 5 (inserta 2 (inserta 3 vacia)))
    `shouldBe` [3, 2, 5]

spec :: Spec
spec = do
  describe "def. listaAcola 1" $ specG1 listaAcola
  describe "def. listaAcola 2" $ specG1 listaAcola2
  describe "def. listaAcola 3" $ specG1 listaAcola3
  describe "def. listaAcola 4" $ specG1 listaAcola4
  describe "def. listaAcola 5" $ specG1 listaAcola5
  describe "def. colaAlista 1" $ specG2 colaAlista
