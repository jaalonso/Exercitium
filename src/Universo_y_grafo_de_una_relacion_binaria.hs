-- Universo_y_grafo_de_una_relacion_binaria.hs
-- Universo y grafo de una relación binaria.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir las siguientes funciones
--    universo :: Eq a => Rel a -> [a]
--    grafo    :: Eq a => ([a],[(a,a)]) -> [(a,a)]
-- tales que
-- + (universo r) es el universo de la relación r. Por ejemplo,
--      λ> r = R ([1, 3],[(3, 1), (3, 3)])
--      λ> universo r
--      [1,3]
-- + (grafo r) es el grafo de la relación r. Por ejemplo,
--      λ> grafo r
--      [(3,1),(3,3)]
-- ---------------------------------------------------------------------

module Universo_y_grafo_de_una_relacion_binaria where

import Relaciones_binarias (Rel(R))

universo :: Eq a => Rel a -> [a]
universo (R (u,_)) = u

grafo :: Eq a => Rel a -> [(a,a)]
grafo (R (_,g)) = g
