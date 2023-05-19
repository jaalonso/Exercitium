module Grafo_Grafos_ciclos_Spec (main, spec) where

import Grafo_Grafos_ciclos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (grafoCiclo 3) `shouldBe`
    "G ND [1,2,3] [(1,2),(1,3),(2,3)]"
