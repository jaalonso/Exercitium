-- Grafo_Grafos_completos.hs
-- TAD de los grafos: Grafos_completos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El grafo completo de orden n, K(n), es un grafo no dirigido cuyos
-- conjunto de vértices es {1,..n} y tiene una arista entre cada par de
-- vértices distintos.
--
-- Usando el [tipo abstrado de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    completo :: Int -> Grafo Int Int
-- tal que (completo n) es el grafo completo de orden n. Por ejemplo,
--    λ> completo 4
--    G ND ([1,2,3,4],[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
-- ---------------------------------------------------------------------

module Grafo_Grafos_completos where

import TAD.Grafo (Grafo, Orientacion (ND), creaGrafo')
import Test.Hspec (Spec, hspec, it, shouldBe)

completo :: Int -> Grafo Int Int
completo n =
  creaGrafo' ND (1,n) [(x,y) | x <- [1..n], y <- [x+1..n]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (completo 4) `shouldBe`
    "G ND ([1,2,3,4],[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])"

-- La verificación es
--    λ> verifica
--
--    e1
--
--    Finished in 0.0004 seconds
--    1 example, 0 failures
