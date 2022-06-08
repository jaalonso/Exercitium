module Menor_numero_con_una_cantidad_dada_de_divisores_Spec (main, spec) where

import Menor_numero_con_una_cantidad_dada_de_divisores
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG menor' = do
  it "e1" $
    menor' 1  `shouldBe`  2
  it "e2" $
    menor' 2  `shouldBe`  6
  it "e3" $
    menor' 3  `shouldBe`  24
  it "e4" $
    menor' 4  `shouldBe`  120

spec :: Spec
spec = do
  describe "def. 1" $ specG menor1
  describe "def. 2" $ specG menor2
  describe "def. 3" $ specG menor3
