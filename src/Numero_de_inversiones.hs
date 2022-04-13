-- Numero_de_inversiones.hs
-- Número de inversiones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se dice que en una sucesión de números x(1), x(2), ..., x(n) hay una
-- inversión cuando existe un par de números x(i) > x(j), siendo i < j.
-- Por ejemplo, en la permutación 2, 1, 4, 3 hay dos inversiones
-- (2 antes que 1 y 4 antes que 3) y en la permutación 4, 3, 1, 2 hay
-- cinco inversiones (4 antes 3, 4 antes 1, 4 antes 2, 3 antes 1,
-- 3 antes 2).
--
-- Definir la función
--    numeroInversiones :: Ord a => [a] -> Int
-- tal que (numeroInversiones xs) es el número de inversiones de xs. Por
-- ejemplo,
--    numeroInversiones [2,1,4,3]  ==  2
--    numeroInversiones [4,3,1,2]  ==  5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numero_de_inversiones where

import Test.QuickCheck
import Data.Array

-- 1ª solución
-- ===========

numeroInversiones1 :: Ord a => [a] -> Int
numeroInversiones1 = length . indicesInversiones

-- (indicesInversiones xs) es la lista de los índices de las inversiones
-- de xs. Por ejemplo,
--    indicesInversiones [2,1,4,3]  ==  [(0,1),(2,3)]
--    indicesInversiones [4,3,1,2]  ==  [(0,1),(0,2),(0,3),(1,2),(1,3)]
indicesInversiones :: Ord a => [a] -> [(Int,Int)]
indicesInversiones xs = [(i,j) | i <- [0..n-2],
                                 j <- [i+1..n-1],
                                 xs!!i > xs!!j]
  where n = length xs

-- 2ª solución
-- ===========

numeroInversiones2 :: Ord a => [a] -> Int
numeroInversiones2 = length . indicesInversiones2

indicesInversiones2 :: Ord a => [a] -> [(Int,Int)]
indicesInversiones2 xs = [(i,j) | i <- [0..n-2],
                                  j <- [i+1..n-1],
                                  v!i > v!j]
  where n = length xs
        v = listArray (0,n-1) xs

-- 3ª solución
-- ===========

numeroInversiones3 :: Ord a => [a] -> Int
numeroInversiones3 = length . inversiones

-- (inversiones xs) es la lista de las inversiones  de xs. Por ejemplo,
--    Inversiones [2,1,4,3]  ==  [(2,1),(4,3)]
--    Inversiones [4,3,1,2]  ==  [(4,3),(4,1),(4,2),(3,1),(3,2)]
inversiones :: Ord a => [a] -> [(a,a)]
inversiones []     = []
inversiones (x:xs) = [(x,y) | y <- xs, y < x] ++ inversiones xs

-- 4ª solución
-- ===========

numeroInversiones4 :: Ord a => [a] -> Int
numeroInversiones4 []     = 0
numeroInversiones4 (x:xs) = length (filter (x>) xs) + numeroInversiones4 xs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numeroInversiones :: [Int] -> Bool
prop_numeroInversiones xs =
  all (== numeroInversiones1 xs)
      [numeroInversiones2 xs,
       numeroInversiones3 xs,
       numeroInversiones4 xs]

-- La comprobación es
--    λ> quickCheck prop_numeroInversiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numeroInversiones1 [1200,1199..1]
--    719400
--    (2.30 secs, 236,976,776 bytes)
--    λ> numeroInversiones2 [1200,1199..1]
--    719400
--    (0.61 secs, 294,538,488 bytes)
--    λ> numeroInversiones3 [1200,1199..1]
--    719400
--    (0.26 secs, 150,543,056 bytes)
--    λ> numeroInversiones4 [1200,1199..1]
--    719400
--    (0.10 secs, 41,274,888 bytes)
--
--    λ> numeroInversiones3 [3000,2999..1]
--    4498500
--    (1.35 secs, 937,186,992 bytes)
--    λ> numeroInversiones4 [3000,2999..1]
--    4498500
--    (0.61 secs, 253,665,928 bytes)
