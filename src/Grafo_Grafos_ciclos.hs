-- Grafo_Grafos_ciclos.hs
-- TAD de los grafos: Grafos ciclo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El ciclo de orden n, C(n), es un grafo no dirigido cuyo conjunto de
-- vértices es {1,...,n} y las aristas son
--    (1,2), (2,3), ..., (n-1,n), (n,1)
--
-- Usando el [tipo abstrado de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    grafoCiclo :: Int -> Grafo Int Int
-- tal que (grafoCiclo n) es el grafo ciclo de orden n. Por ejemplo,
--    λ> grafoCiclo 3
--    G ND ([1,2,3],[(1,2),(1,3),(2,3)])
-- ---------------------------------------------------------------------

module Grafo_Grafos_ciclos where

import TAD.Grafo (Grafo, Orientacion (ND), creaGrafo')
import Test.Hspec (Spec, hspec, it, shouldBe)

grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n =
  creaGrafo' ND (1,n) ((n,1):[(x,x+1) | x <- [1..n-1]])

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (grafoCiclo 3) `shouldBe`
    "G ND ([1,2,3],[(1,2),(1,3),(2,3)])"

-- La verificación es
--    λ> verifica
--
--    e1
--
--    Finished in 0.0006 seconds
--    1 example, 0 failures
