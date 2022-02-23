module Suma_de_fila_del_triangulo_de_los_impares_Spec (main, spec) where

import Suma_de_fila_del_triangulo_de_los_impares
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaFilaTrianguloImpares = do
  it "e1" $
    sumaFilaTrianguloImpares 2 `shouldBe` 8
  it "e2" $
    sumaFilaTrianguloImpares 13 `shouldBe` 2197
  it "e3" $
    sumaFilaTrianguloImpares 19 `shouldBe` 6859
  it "e4" $
    sumaFilaTrianguloImpares 99 `shouldBe` 970299

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaFilaTrianguloImpares1
  describe "def. 2" $ specG sumaFilaTrianguloImpares2
