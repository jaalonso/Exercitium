module Producto_cartesiano_de_dos_conjuntos_Spec (main, spec) where

import Producto_cartesiano_de_dos_conjuntos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> [(Int,Int)]) -> Spec
specG producto =
  it "e1" $
    producto [1,3] [2,4] `shouldBe` [(1,2),(1,4),(3,2),(3,4)]

spec :: Spec
spec = do
  describe "def. 1" $ specG producto1
  describe "def. 2" $ specG producto2
