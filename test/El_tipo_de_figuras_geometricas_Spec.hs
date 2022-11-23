module El_tipo_de_figuras_geometricas_Spec (main, spec) where

import El_tipo_de_figuras_geometricas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Figura -> Float) -> Spec
specG area' = do
  it "e1" $
    area' (Circulo 1)   `shouldBe`  3.1415927
  it "e2" $
    area' (Circulo 2)   `shouldBe`  12.566371
  it "e3" $
    area' (Rect 2 5)    `shouldBe`  10.0
  it "e4" $
    area' (cuadrado 3)  `shouldBe`  9.0

spec :: Spec
spec = do
  describe "def. 1" $ specG area
