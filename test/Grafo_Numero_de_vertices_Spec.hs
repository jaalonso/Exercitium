module Grafo_Numero_de_vertices_Spec (main, spec) where

import Grafo_Numero_de_vertices
import TAD.Grafo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    nVertices (creaGrafo' D (1,5) [(1,2),(3,1)])  `shouldBe` 5
  it "e2" $
    nVertices (creaGrafo' ND (2,4) [(1,2),(3,1)]) `shouldBe` 3
