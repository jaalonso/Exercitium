-- Grafo_Numero_de_vertices.hs
-- TAD de los grafos: Número de vértices.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstrado de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    nVertices :: (Ix v, Num p) => Grafo v p ->  Int
-- tal que (nVertices g) es el número de vértices del grafo g. Por
-- ejemplo,
--    nVertices (creaGrafo' D (1,5) [(1,2),(3,1)])   ==  5
--    nVertices (creaGrafo' ND (0,5) [(1,2),(3,1)])  ==  6
-- ---------------------------------------------------------------------

module Grafo_Numero_de_vertices where

import TAD.Grafo (Grafo, Orientacion (D, ND), nodos, creaGrafo')
import Data.Ix (Ix)
import Test.Hspec (Spec, hspec, it, shouldBe)

nVertices :: (Ix v, Num p) => Grafo v p ->  Int
nVertices = length . nodos

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    nVertices (creaGrafo' D (1,5) [(1,2),(3,1)] :: Grafo Int Int)  `shouldBe` 5
  it "e2" $
    nVertices (creaGrafo' ND (0,5) [(1,2),(3,1)] :: Grafo Int Int) `shouldBe` 6

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0002 seconds
--    2 examples, 0 failures
