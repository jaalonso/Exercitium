module Numero_de_representaciones_de_n_como_suma_de_dos_cuadrados_Spec (main, spec) where

import Numero_de_representaciones_de_n_como_suma_de_dos_cuadrados
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG nRepresentaciones = do
  it "e1" $
    nRepresentaciones (2^3*3^9*5^3*7^8*13^6) `shouldBe` 0
  it "e2" $
    nRepresentaciones (2^3*3^2*5^3*7^8*13^6) `shouldBe` 14

spec :: Spec
spec = do
  describe "def. 1" $ specG nRepresentaciones1
  describe "def. 2" $ specG nRepresentaciones2
  describe "equivalencia" $ it "p1" $ property prop_nRepresentaciones
