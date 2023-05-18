-- Grafo_Grafos_completos.hs
-- Grafos completos.
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
--    G ND ([1,2,3,4],
--          [((1,2),0),((1,3),0),((1,4),0),
--           ((2,1),0),((2,3),0),((2,4),0),
--           ((3,1),0),((3,2),0),((3,4),0),
--           ((4,1),0),((4,2),0),((4,3),0)])
-- ---------------------------------------------------------------------

module Grafo_Grafos_completos where

import TAD.Grafo (Grafo, Orientacion (ND), creaGrafo)

completo :: Int -> Grafo Int Int
completo n =
  creaGrafo ND (1,n) [(x,y,0) | x <- [1..n], y <- [x+1..n]]
