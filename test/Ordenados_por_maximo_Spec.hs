module Ordenados_por_maximo_Spec (main, spec) where

import Ordenados_por_maximo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "e1" $
      ordenadosPorMaximo1 [[0,8],[9],[8,1],[6,3],[8,2],[6,1],[6,2]] `shouldBe`
      [[6,3],[6,1],[6,2],[0,8],[8,1],[8,2],[9]]
    it "e2" $
      ordenadosPorMaximo1 ["este","es","el","primero"] `shouldBe`
      ["el","primero","es","este"]
    it "e3" $
      property prop_ordenadosPorMaximo
