module Polinomios_cuadraticos_generadores_de_primos_Spec (main, spec) where

import Polinomios_cuadraticos_generadores_de_primos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> (Int,[(Integer,Integer)])) -> Spec
specG generadoresMaximales = do
  it "e1" $
    equivalentes (generadoresMaximales 4) (3,[(-2,3),(-1,3),(3,3)])
  it "e2" $
    equivalentes (generadoresMaximales 6) (5,[(-1,5),(5,5)])

spec :: Spec
spec = do
  describe "def. 1" $ specG generadoresMaximales1
  describe "def. 2" $ specG generadoresMaximales2
  describe "def. 3" $ specG generadoresMaximales3
  describe "def. 4" $ specG generadoresMaximales4
