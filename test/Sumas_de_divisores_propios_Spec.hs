module Sumas_de_divisores_propios_Spec (main, spec) where

import Sumas_de_divisores_propios
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [(Integer,Integer)]) -> Spec
specG sumaDivisoresHasta = do
  it "e1" $
    sumaDivisoresHasta 12 `shouldBe`
    [(1,0),(2,1),(3,1),(4,3),(5,1),(6,6),(7,1),(8,7),(9,4),(10,8),(11,1),(12,16)]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDivisoresHasta1
  describe "def. 2" $ specG sumaDivisoresHasta2
  describe "def. 3" $ specG sumaDivisoresHasta3
  describe "equivalencia" $ it "p1" $ property prop_sumaDivisoresHasta
