module El_tipo_de_los_arboles_binarios_Spec (main, spec) where

import El_tipo_de_los_arboles_binarios
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    ocurre 4  ejArbol  `shouldBe`  True
  it "e2" $
    ocurre 10 ejArbol  `shouldBe`  False
  it "e3" $
    aplana ejArbol  `shouldBe`  [1,3,4,5,6,7,9]
