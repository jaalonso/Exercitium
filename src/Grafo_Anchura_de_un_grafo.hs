-- Grafo_Anchura_de_un_grafo.hs
-- TAD de los grafos: Anchura de un grafo,
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En un grafo, la anchura de un nodo es el máximo de los valores
-- absolutos de la diferencia entre el valor del nodo y los de sus
-- adyacentes; y la anchura del grafo es la máxima anchura de sus
-- nodos. Por ejemplo, en el grafo
--    grafo1 :: Grafo Int Int
--    grafo1 = creaGrafo' D (1,5) [(1,2),(1,3),(1,5),
--                                 (2,4),(2,5),
--                                 (3,4),(3,5),
--                                 (4,5)]
-- su anchura es 4 y el nodo de máxima anchura es el 5.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    anchura :: Grafo Int Int -> Int
-- tal que (anchuraG g) es la anchura del grafo g. Por ejemplo,
--    anchura grafo1  ==  4
--
-- Comprobar experimentalmente que la anchura del grafo ciclo de orden
-- n es n-1.
-- ---------------------------------------------------------------------

module Grafo_Anchura_de_un_grafo where

import TAD.Grafo (Grafo, Orientacion (D, ND), adyacentes, aristas,
                  creaGrafo', nodos)
import Grafo_Grafos_ciclos (grafoCiclo)
import Test.Hspec (Spec, hspec, it, shouldBe)

grafo1 :: Grafo Int Int
grafo1 = creaGrafo' D (1,5) [(1,2),(1,3),(1,5),
                             (2,4),(2,5),
                             (3,4),(3,5),
                             (4,5)]

-- 1ª solución
-- ===========

anchura :: Grafo Int Int -> Int
anchura g = maximum [anchuraN g x | x <- nodos g]

-- (anchuraN g x) es la anchura del nodo x en el grafo g. Por ejemplo,
--    anchuraN g 1  ==  4
--    anchuraN g 2  ==  3
--    anchuraN g 4  ==  2
--    anchuraN g 5  ==  4
anchuraN :: Grafo Int Int -> Int -> Int
anchuraN g x = maximum (0 : [abs (x-v) | v <- adyacentes g x])

-- 2ª solución
-- ===========

anchura2 :: Grafo Int Int -> Int
anchura2 g = maximum [abs (x-y) | ((x,y),_) <- aristas g]

-- La conjetura
conjetura :: Int -> Bool
conjetura n = anchura (grafoCiclo n) == n-1

-- La comprobación es
--    λ> and [conjetura n | n <- [2..10]]
--    True

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    anchura grafo1 `shouldBe` 4
  it "e2" $
    anchura g2 `shouldBe` 2
  where
    g2 :: Grafo Int Int
    g2 = creaGrafo' ND (1,3) [(1,2),(1,3),(2,3),(3,3)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0004 seconds
--    2 examples, 0 failures
