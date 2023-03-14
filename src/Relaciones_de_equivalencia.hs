-- Relaciones_de_equivalencia.hs
-- Relaciones de equivalencia.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla,  06-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    esEquivalencia :: Ord a => Rel a -> Bool
-- tal que (esEquivalencia r) se verifica si la relación r es de
-- equivalencia. Por ejemplo,
--    λ> esEquivalencia (R ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)]))
--    True
--    λ> esEquivalencia (R ([1,2,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)]))
--    False
--    λ> esEquivalencia (R ([1,3,5],[(1,1),(1,3),(3,3),(5,5)]))
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_de_equivalencia where

import Relaciones_binarias (Rel(R))
import Relaciones_reflexivas (reflexiva)
import Relaciones_simetricas (simetrica)
import Relaciones_transitivas (transitiva)

esEquivalencia :: Ord a => Rel a -> Bool
esEquivalencia r = reflexiva r && simetrica r && transitiva r
