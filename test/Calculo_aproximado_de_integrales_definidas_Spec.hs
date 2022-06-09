module Calculo_aproximado_de_integrales_definidas_Spec (main, spec) where

import Calculo_aproximado_de_integrales_definidas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Double -> Double -> (Double -> Double) -> Double -> Double) -> Spec
specG integral = do
  it "e1" $
    integral 0 1 (^3) 0.01 =~ 0.24998750000000042 
  it "e2" $
    integral 0 1 (^4) 0.01 =~ 0.19998333362500048 
  it "e3" $
    integral 0 1 (\x -> 3*x^2 + 4*x^3) 0.01 =~ 1.9999250000000026 
  it "e4" $
    (log 2 - integral 1 2 (\x -> 1/x) 0.01) =~ 3.124931644782336e-6 
  it "e5" $
    (pi - 4 * integral 0 1 (\x -> 1/(x^2+1)) 0.01) =~ (-8.333333331389525e-6)

(=~) :: Double -> Double -> Bool
x =~ y = abs (x - y) < 0.001

  
spec :: Spec
spec = do
  describe "def. 1" $ specG integral1
  describe "def. 2" $ specG integral2
  describe "def. 3" $ specG integral3
