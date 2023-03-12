-- Relaciones_transitivas.hs
-- Relaciones transitivas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    transitiva :: Eq a => Rel a -> Bool
-- tal que (transitiva r) se verifica si la relación r es transitiva.
-- Por ejemplo,
--    transitiva ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])  ==  True
--    transitiva ([1,3,5],[(1,1),(1,3),(3,1),(5,5)])        ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_transitivas where

import Relaciones_binarias (Rel(R))
import Reconocimiento_de_subconjunto (subconjunto)
import Universo_y_grafo_de_una_relacion_binaria (grafo)
import Test.QuickCheck

-- 1ª solución
-- ===========

transitiva :: Eq a => Rel a -> Bool
transitiva r@(_,ps) = subconjunto (grafo (composicion r r)) ps

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
