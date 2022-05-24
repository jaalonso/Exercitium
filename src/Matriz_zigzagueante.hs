-- Matriz_zigzagueante.hs
-- Matriz zigzagueante.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La matriz zizagueante de orden n es la matriz cuadrada con n filas y
-- n columnas y cuyos elementos son los n² primeros números naturales
-- colocados de manera creciente a lo largo de las diagonales
-- secundarias. Por ejemplo, La matriz zigzagueante de orden 5 es
--     0  1  5  6 14
--     2  4  7 13 15
--     3  8 12 16 21
--     9 11 17 20 22
--    10 18 19 23 24
-- La colocación de los elementos se puede ver gráficamente en
-- http://bit.ly/1DeO2FI
--
-- Definir la función
--    zigZag :: oInt -> Matrix Int
-- tal que (zigZag n) es la matriz zigzagueante de orden n. Por ejemplo,
--    λ> zigZag1 5
--    ┌                ┐
--    │  0  1  5  6 14 │
--    │  2  4  7 13 15 │
--    │  3  8 12 16 21 │
--    │  9 11 17 20 22 │
--    │ 10 18 19 23 24 │
--    └                ┘
--    λ> zigZag1 4
--    ┌             ┐
--    │  0  1  5  6 │
--    │  2  4  7 12 │
--    │  3  8 11 13 │
--    │  9 10 14 15 │
--    └             ┘
--    λ> maximum (zigZag 1500)
--    2249999
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Matriz_zigzagueante where

import Data.List (sort, sortBy)
import Data.Matrix (Matrix, fromList)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

zigZag1 :: Int -> Matrix Int
zigZag1 n = fromList n n (elementosZigZag n)

-- (elementosZigZag n) es la lista de los elementos de la matriz
-- zizagueante de orden n. Por ejemplo.
--    λ> elementosZigZag 5
--    [0,1,5,6,14,2,4,7,13,15,3,8,12,16,21,9,11,17,20,22,10,18,19,23,24]
elementosZigZag :: Int -> [Int]
elementosZigZag n =
  map snd (sort (zip (ordenZigZag n) [0..]))

-- (ordenZigZag n) es la lista de puntos del cuadrado nxn recorridos en
-- zig-zag por las diagonales secundarias. Por ejemplo,
--    λ> ordenZigZag 4
--    [(1,1), (1,2),(2,1), (3,1),(2,2),(1,3), (1,4),(2,3),(3,2),(4,1),
--     (4,2),(3,3),(2,4), (3,4),(4,3), (4,4)]
ordenZigZag :: Int -> [(Int,Int)]
ordenZigZag n = concat [aux n m | m <- [2..2*n]]
    where aux k m | odd m     = [(x,m-x) | x <- [max 1 (m-k)..min k (m-1)]]
                  | otherwise = [(m-x,x) | x <- [max 1 (m-k)..min k (m-1)]]

-- 2ª solución
-- ===========

zigZag2 :: Int -> Matrix Int
zigZag2 n = fromList n n (elementosZigZag2 n)

elementosZigZag2 :: Int -> [Int]
elementosZigZag2 n =
  map snd (sort (zip (ordenZigZag2 n) [0..]))

ordenZigZag2 :: Int -> [(Int,Int)]
ordenZigZag2 n = sortBy comp [(x,y) | x <- [1..n], y <- [1..n]]
    where comp (x1,y1) (x2,y2) | x1+y1 < x2+y2 = LT
                               | x1+y1 > x2+y2 = GT
                               | even (x1+y1)  = compare y1 y2
                               | otherwise     = compare x1 x2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_zigZag :: Positive Int -> Bool
prop_zigZag (Positive n) =
  zigZag1 n == zigZag2 n

-- La comprobación es
--    λ> quickCheck prop_zigZag
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (zigZag1 5000)
--    25000000
--    (2.57 secs, 1,800,683,952 bytes)
--    λ> length (zigZag2 5000)
--    25000000
--    (2.20 secs, 1,800,683,952 bytes)
--
--    λ> maximum (zigZag1 1100)
--    1209999
--    (2.12 secs, 1,840,095,864 bytes)
--    λ> maximum (zigZag2 1100)
--    1209999
--    (21.27 secs, 11,661,088,256 bytes)
