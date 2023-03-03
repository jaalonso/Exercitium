-- Universo_y_grafo_de_una_relacion_binaria.hs
-- Universo y grafo de una relación binaria.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una relación binaria R sobre un conjunto A se puede representar
-- mediante un par (xs,ps) donde xs es la lista de los elementos de A
-- (el universo de R) y ps es la lista de pares de R (el grafo de R).
--
-- Definir el tipo de dato (Rel a), para representar las relaciones
-- binarias sobre a, y las siguientes funciones
--    universo :: Eq a => Rel a -> [a]
--    grafo    :: Eq a => ([a],[(a,a)]) -> [(a,a)]
-- tales que
-- + (universo r) es el universo de la relación r. Por ejemplo,
--      λ> r = ([1..9],[(1,3),(2,6),(8,9),(2,7)])
--      λ> universo r
--      [1,2,3,4,5,6,7,8,9]
-- + (grafo r) es el grafo de la relación r. Por ejemplo,
--      λ> grafo r
--      [(1,3),(2,6),(8,9),(2,7)]
-- ---------------------------------------------------------------------

module Universo_y_grafo_de_una_relacion_binaria where

type Rel a = ([a],[(a,a)])

universo :: Eq a => Rel a -> [a]
universo (us,_) = us

grafo :: Eq a => Rel a -> [(a,a)]
grafo (_,ps) = ps
