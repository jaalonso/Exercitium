module Suma_de_numeros_amigos_menores_que_n_Spec (main, spec) where

import Suma_de_numeros_amigos_menores_que_n
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaAmigosMenores = do
  it "e1" $
    sumaAmigosMenores 1000   `shouldBe` 504

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaAmigosMenores1
  describe "def. 2" $ specG sumaAmigosMenores2
  describe "def. 3" $ specG sumaAmigosMenores3
  describe "def. 4" $ specG sumaAmigosMenores4
  describe "def. 5" $ specG sumaAmigosMenores5
  describe "def. 6" $ specG sumaAmigosMenores6
  describe "def. 7" $ specG sumaAmigosMenores7
