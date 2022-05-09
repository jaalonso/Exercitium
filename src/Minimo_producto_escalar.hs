-- Minimo_producto_escalar.hs
-- Mínimo producto escalar.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El producto escalar de los vectores [a1,a2,...,an] y [b1,b2,..., bn]
-- es
--    a1 * b1 + a2 * b2 + ··· + an * bn.
--
-- Definir la función
--    menorProductoEscalar :: (Ord a, Num a) => [a] -> [a] -> a
-- tal que (menorProductoEscalar xs ys) es el mínimo de los productos
-- escalares de las permutaciones de xs y de las permutaciones de
-- ys. Por ejemplo,
--    menorProductoEscalar [3,2,5]  [1,4,6]    == 29
--    menorProductoEscalar [3,2,5]  [1,4,-6]   == -19
--    menorProductoEscalar [1..10^2] [1..10^2] == 171700
--    menorProductoEscalar [1..10^3] [1..10^3] == 167167000
--    menorProductoEscalar [1..10^4] [1..10^4] == 166716670000
--    menorProductoEscalar [1..10^5] [1..10^5] == 166671666700000
--    menorProductoEscalar [1..10^6] [1..10^6] == 166667166667000000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Minimo_producto_escalar where

import Data.List (sort, permutations)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

menorProductoEscalar1 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar1 xs ys =
  minimum [sum (zipWith (*) pxs pys) | pxs <- permutations xs,
                                       pys <- permutations ys]

-- 2ª solución
-- ===========

menorProductoEscalar2 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar2 xs ys =
  minimum [sum (zipWith (*) pxs ys) | pxs <- permutations xs]

-- 3ª solución
-- ===========

menorProductoEscalar3 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar3 xs ys =
  sum (zipWith (*) (sort xs) (reverse (sort ys)))

-- Equivalencia
-- ============

-- La propiedad es
prop_menorProductoEscalar :: [Integer] -> [Integer] -> Bool
prop_menorProductoEscalar xs ys =
  all (== menorProductoEscalar1 xs' ys')
      [menorProductoEscalar2 xs' ys',
       menorProductoEscalar3 xs' ys']
  where n   = min (length xs) (length ys)
        xs' = take n xs
        ys' = take n ys

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_menorProductoEscalar
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> menorProductoEscalar1 [0..5] [0..5]
--    20
--    (3.24 secs, 977385528 bytes)
--    λ> menorProductoEscalar2 [0..5] [0..5]
--    20
--    (0.01 secs, 4185776 bytes)
--
--    λ> menorProductoEscalar2 [0..9] [0..9]
--    120
--    (23.86 secs, 9342872784 bytes)
--    λ> menorProductoEscalar3 [0..9] [0..9]
--    120
--    (0.01 secs, 2580824 bytes)
--
--    λ> menorProductoEscalar3 [0..10^6] [0..10^6]
--    166666666666500000
--    (2.46 secs, 473,338,912 bytes)
