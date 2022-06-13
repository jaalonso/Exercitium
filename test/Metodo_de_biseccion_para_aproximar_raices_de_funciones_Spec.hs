module Metodo_de_biseccion_para_aproximar_raices_de_funciones_Spec (main, spec) where

import Metodo_de_biseccion_para_aproximar_raices_de_funciones
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Double -> Double) -> Double -> Double -> Double -> Double) -> Spec
specG biseccion = do
  it "e1" $
    biseccion (\x -> x^2 - 3) 0 5 0.01             `shouldBe`  1.7333984375 
  it "e2" $
    biseccion (\x -> x^3 - x - 2) 0 4 0.01         `shouldBe`  1.521484375 
  it "e3" $
    biseccion cos 0 2 0.01                         `shouldBe`  1.5625 
  it "e4" $
    biseccion (\x -> log (50-x) - 4) (-10) 3 0.01  `shouldBe`  -5.125

spec :: Spec
spec = do
  describe "def. 1" $ specG biseccion1
  describe "def. 2" $ specG biseccion2
