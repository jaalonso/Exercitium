-- Grafo_Incidentes_de_un_vertice.hs
-- TAD de los grafos: Incidentes de un vértice.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En un un grafo g, los incidentes de un vértice v es el conjuntos de
-- vértices x de g para los que hay un arco (o una arista) de x a v; es
-- decir, que v es adyacente a x.
--
-- Usando el [tipo abstrado de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    incidentes :: (Ix v,Num p) => (Grafo v p) -> v -> [v]
-- tal que (incidentes g v) es la lista de los vértices incidentes en el
-- vértice v. Por ejemplo,
--    λ> g1 = creaGrafo' D (1,3) [(1,2),(2,2),(3,1),(3,2)]
--    λ> incidentes g1 1
--    [3]
--    λ> incidentes g1 2
--    [1,2,3]
--    λ> incidentes g1 3
--    []
--    λ> g2 = creaGrafo' ND (1,3) [(1,2),(2,2),(3,1),(3,2)]
--    λ> incidentes g2 1
--    [2,3]
--    λ> incidentes g2 2
--    [1,2,3]
--    λ> incidentes g2 3
--    [1,2]
-- ---------------------------------------------------------------------

module Grafo_Incidentes_de_un_vertice where

import TAD.Grafo (Grafo, Orientacion (D, ND), nodos,  adyacentes, creaGrafo')
import Data.Ix
import Test.Hspec

incidentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes g v = [x | x <- nodos g, v `elem` adyacentes g x]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

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
    g1, g2 :: Grafo Int Int
    g1 = creaGrafo' D (1,3) [(1,2),(2,2),(3,1),(3,2)]
    g2 = creaGrafo' ND (1,3) [(1,2),(2,2),(3,1),(3,2)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--
--    Finished in 0.0005 seconds
--    6 examples, 0 failures
