module Grafo_Grafos_completos_Spec (main, spec) where

import Grafo_Grafos_completos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (completo 4) `shouldBe`
    "G ND ([1,2,3,4],[((1,2),0),((1,3),0),((1,4),0),((2,1),0),((2,3),0),((2,4),0),((3,1),0),((3,2),0),((3,4),0),((4,1),0),((4,2),0),((4,3),0)])"
