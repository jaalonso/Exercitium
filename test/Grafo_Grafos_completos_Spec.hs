module Grafo_Grafos_completos_Spec (main, spec) where

import Grafo_Grafos_completos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (completo 4) `shouldBe`
    "G ND [1,2,3,4] [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]"
