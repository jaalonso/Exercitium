module Grafo_Incidentes_de_un_vertice_Spec (main, spec) where

import Grafo_Incidentes_de_un_vertice
import TAD.Grafo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    incidentes g1 1 `shouldBe` [3]
  it "e2" $
    incidentes g1 2 `shouldBe` [1,2,3]
  it "e3" $
    incidentes g1 3 `shouldBe` []
  it "e4" $
    incidentes g2 1 `shouldBe` [2,3]
  it "e5" $
    incidentes g2 2 `shouldBe` [1,2,3]
  it "e6" $
    incidentes g2 3 `shouldBe` [1,2]
  where
    g1 = creaGrafo' D (1,3) [(1,2),(2,2),(3,1),(3,2)]
    g2 = creaGrafo' ND (1,3) [(1,2),(2,2),(3,1),(3,2)]
