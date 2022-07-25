module Representaciones_de_un_numero_como_suma_de_dos_cuadrados_Spec (main, spec) where

import Representaciones_de_un_numero_como_suma_de_dos_cuadrados
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [(Integer,Integer)]) -> Spec
specG representaciones = do
  it "e1" $
    representaciones  20 `shouldBe` [(2,4)]
  it "e2" $
    representaciones  25 `shouldBe` [(0,5),(3,4)]
  it "e3" $
    representaciones 325 `shouldBe` [(1,18),(6,17),(10,15)]

spec :: Spec
spec = do
  describe "def. 1" $ specG representaciones1
  describe "def. 2" $ specG representaciones2
  describe "def. 3" $ specG representaciones3
  describe "equivalencia" $ it "p1" $ property prop_representaciones
