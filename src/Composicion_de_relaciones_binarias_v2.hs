-- Composicion_de_relaciones_binarias_v2.hs
-- Composición de relaciones binarias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 03-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    composicion :: Eq a => Rel a -> Rel a -> Rel a
-- tal que (composicion r s) es la composición de las relaciones r y
-- s. Por ejemplo,
--    λ> composicion (R ([1,2],[(1,2),(2,2)])) (R ([1,2],[(2,1)]))
--    R ([1,2],[(1,1),(2,1)])
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Composicion_de_relaciones_binarias_v2 where

import Relaciones_binarias (Rel(R))
import Test.QuickCheck

-- 1ª solución
-- ===========

composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R (u1,g1)) (R (_,g2)) =
  R (u1,[(x,z) | (x,y) <- g1, (y',z) <- g2, y == y'])

-- 2ª solución
-- ===========

composicion2 :: Eq a => Rel a -> Rel a -> Rel a
composicion2 (R (u1,g1)) (R (_,g2)) =
  R (u1, aux g1)
  where aux [] = []
        aux ((x,y):g1') = [(x,z) | (y',z) <- g2, y == y'] ++ aux g1'

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_composicion :: Rel Int -> Rel Int -> Bool
prop_composicion r1 r2 =
  composicion r1 r2 == composicion2 r1 r2

-- La comprobación es
--    λ> quickCheck prop_composicion
--    +++ OK, passed 100 tests.
