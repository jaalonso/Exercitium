-- Clausura_transitiva.hs
-- Clausura transitiva.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    clausuraTransitiva :: Eq a => Rel a -> Rel a
-- tal que (clausuraTransitiva r) es la clausura transitiva de r; es
-- decir, la menor relación transitiva que contiene a r. Por ejemplo,
--    λ> clausuraTransitiva (R ([1..6],[(1,2),(2,5),(5,6)]))
--    R ([1,2,3,4,5,6],[(1,2),(2,5),(5,6),(1,5),(2,6),(1,6)])
--
-- Comprobar con QuickCheck que clausuraTransitiva es transitiva.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Clausura_transitiva where

import Relaciones_binarias (Rel(R))
import Relaciones_transitivas (transitiva)
import Data.List (union)
import Test.QuickCheck

--  1ª solución
--  ===========

clausuraTransitiva :: Ord a => Rel a -> Rel a
clausuraTransitiva (R (u,g)) = R (u, aux g)
  where aux u' | cerradoTr u' = u'
               | otherwise    = aux (u' `union` comp u' u')
        cerradoTr r       = subconjunto (comp r r) r
        comp r s          = [(x,z) | (x,y) <- r, (y',z) <- s, y == y']
        subconjunto xs ys = all (`elem` ys) xs

-- 2ª solución
-- ===========

clausuraTransitiva2 :: Ord a => Rel a -> Rel a
clausuraTransitiva2 (R (u,g)) =
  R (u, until cerradoTr (\r -> r `union` comp r r) g)
  where
    cerradoTr r       = subconjunto (comp r r) r
    comp r s          = [(x,z) | (x,y) <- r, (y',z) <- s, y == y']
    subconjunto xs ys = all (`elem` ys) xs


-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_clausuraTransitiva :: Rel Int -> Bool
prop_clausuraTransitiva r =
  clausuraTransitiva r == clausuraTransitiva2 r

-- La comprobación es
--    λ> quickCheck prop_clausuraTransitiva
--    +++ OK, passed 100 tests.

-- Propiedad
-- =========

-- La propiedad es
prop_clausuraTransitivaEsTransitiva :: Rel Int -> Bool
prop_clausuraTransitivaEsTransitiva r =
  transitiva (clausuraTransitiva r)

-- La función transitiva está definida en el ejercicio
-- "Relaciones transitivas" que se encuentra en
-- https://bit.ly/42WRPJv

-- La comprobación es
--    λ> quickCheck prop_clausuraTransitivaEsTransitiva
--    +++ OK, passed 100 tests.
