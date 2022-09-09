module Suma_de_cuadrados_menos_cuadrado_de_la_suma_Spec (main, spec) where

import Suma_de_cuadrados_menos_cuadrado_de_la_suma
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer-> Integer) -> Spec
specG euler6 =
  it "e1" $
    euler6 10 `shouldBe` 2640

spec :: Spec
spec = do
  describe "def. 1" $ specG euler6a
  describe "def. 2" $ specG euler6b
  describe "def. 3" $ specG euler6c
  describe "def. 4" $ specG euler6d
  describe "def. 5" $ specG euler6e
  describe "equivalencia" $ it "p1" $ property prop_euler6
