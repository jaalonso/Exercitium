module Mayor_producto_de_las_ramas_de_un_arbol_Spec (main, spec) where

import Mayor_producto_de_las_ramas_de_un_arbol
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Arbol Integer -> Integer) -> Spec
specG mayorProducto = do
  it "e1" $
    mayorProducto (N 1 [N 2 [], N  3 []]) `shouldBe`3
  it "e2" $
    mayorProducto (N 1 [N 8 [], N  4 [N 3 []]]) `shouldBe`12
  it "e3" $
    mayorProducto (N 1 [N 2 [],N 3 [N 4 []]]) `shouldBe`12
  it "e4" $
    mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
    `shouldBe` 90
  it "e5" $
    mayorProducto (N (-8) [N 0 [N (-9) []],N 6 []]) `shouldBe` 0
  it "e6" $ do
    let a = N (-4) [N (-7) [],N 14 [N 19 []],N (-1) [N (-6) [],N 21 []],N (-4) []]
    mayorProducto a `shouldBe` 84

spec :: Spec
spec = do
  describe "def. 1" $ specG mayorProducto1
  describe "def. 2" $ specG mayorProducto2
  describe "def. 3" $ specG mayorProducto3
  describe "def. 4" $ specG mayorProducto4
  describe "def. 5" $ specG mayorProducto5
  describe "def. 6" $ specG mayorProducto6
