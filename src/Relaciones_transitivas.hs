-- Relaciones_transitivas.hs
-- Relaciones transitivas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    transitiva :: Ord a => Rel a -> Bool
-- tal que (transitiva r) se verifica si la relación r es transitiva.
-- Por ejemplo,
--    transitiva (R ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])) == True
--    transitiva (R ([1,3,5],[(1,1),(1,3),(3,1),(5,5)]))       == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_transitivas where

import Relaciones_binarias (Rel(R))
import Reconocimiento_de_subconjunto (subconjunto)
import Universo_y_grafo_de_una_relacion_binaria (grafo)
import Composicion_de_relaciones_binarias_v2 (composicion)
import Test.QuickCheck

-- 1ª solución
-- ===========

transitiva1 :: Ord a => Rel a -> Bool
transitiva1 r@(R (_,g)) = subconjunto (grafo (composicion r r)) g

-- La función subconjunto está definida en el ejercicio
-- "Reconocimiento de subconjunto" que se encuentra en
-- https://bit.ly/427Tyeq
--
-- La función grafo está definida en el ejercicio
-- "Universo y grafo de una relación binaria" que se encuentra en
-- https://bit.ly/3J35mpC
--
-- La función composición está definida en el ejercicio
-- "Composición de relaciones binarias" que se encuentra en
-- https://bit.ly/3JyJrs7

-- 2ª solución
-- ===========

transitiva2 :: Ord a => Rel a -> Bool
transitiva2 (R (_,g)) = aux g
  where
    aux [] = True
    aux ((x,y):g') = and [(x,z) `elem` g | (u,z) <- g, u == y] && aux g'

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_transitiva :: Rel Int -> Bool
prop_transitiva r =
  transitiva1 r == transitiva2 r

-- La comprobación es
--    λ> quickCheck prop_transitiva
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> transitiva1 (R ([1..4001],[(x,x+1) | x <- [1..4000]]))
--    False
--    (3.15 secs, 898,932,776 bytes)
--    λ> transitiva2 (R ([1..4001],[(x,x+1) | x <- [1..4000]]))
--    False
--    (0.01 secs, 1,396,720 bytes)
--
--    λ> transitiva1 (R ([1..60], [(x,y) | x <- [1..60], y <- [1..60]]))
--    True
--    (2.71 secs, 852,578,456 bytes)
--    λ> transitiva2 (R ([1..60], [(x,y) | x <- [1..60], y <- [1..60]]))
--    True
--    (9.13 secs, 777,080,288 bytes)

-- En lo sucesivo, usaremos la 1ª definición
transitiva :: Ord a => Rel a -> Bool
transitiva = transitiva1
