module Familias_de_numeros_con_algun_digito_en_comun_Spec (main, spec) where

import Familias_de_numeros_con_algun_digito_en_comun
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer] -> Bool) -> Spec
specG esFamilia = do
  it "e1" $
    esFamilia [72, 32, 25, 22]  `shouldBe`  True
  it "e2" $
    esFamilia [123,245,568]     `shouldBe`  False
  it "e3" $
    esFamilia [72, 32, 25, 223] `shouldBe`  False
  it "e4" $
    esFamilia [56]              `shouldBe`  True
  it "e5" $
    esFamilia []                `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG esFamilia1
  describe "def. 2" $ specG esFamilia2
  describe "def. 3" $ specG esFamilia3
