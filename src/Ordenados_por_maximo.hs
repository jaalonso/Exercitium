-- Ordenados_por_maximo.hs
-- Ordenación por el máximo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
-- tal que (ordenadosPorMaximo xss) es la lista de los elementos de xss
-- ordenada por sus máximos (se supone que los elementos de xss son
-- listas no vacía) y cuando tiene el mismo máximo se conserva el orden
-- original. Por ejemplo,
--    λ> ordenadosPorMaximo [[0,8],[9],[8,1],[6,3],[8,2],[6,1],[6,2]]
--    [[6,3],[6,1],[6,2],[0,8],[8,1],[8,2],[9]]
--    λ> ordenadosPorMaximo ["este","es","el","primero"]
--    ["el","primero","es","este"]
-- ---------------------------------------------------------------------

module Ordenados_por_maximo where

import Data.List (sort, sortBy)
import GHC.Exts (sortWith)
import Test.QuickCheck (quickCheck)

-- 1ª solución
ordenadosPorMaximo1 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo1 xss =
  map snd (sort [((maximum xs,k),xs) | (k,xs) <- zip [0..] xss])

-- 2ª solución
ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
  [xs | (_,xs) <- sort [((maximum xs,k),xs) | (k,xs) <- zip [0..] xss]]

-- 3ª solución
ordenadosPorMaximo3 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo3 =
  sortBy (\xs ys -> compare (maximum xs) (maximum ys))

-- 4ª solución
ordenadosPorMaximo4 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo4 = sortWith maximum

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_ordenadosPorMaximo :: [[Int]] -> Bool
prop_ordenadosPorMaximo xss =
  all (== ordenadosPorMaximo1 yss)
      [ordenadosPorMaximo2 yss,
       ordenadosPorMaximo3 yss,
       ordenadosPorMaximo4 yss]
  where yss = filter (not . null) xss

verifica_ordenadosPorMaximo :: IO ()
verifica_ordenadosPorMaximo =
  quickCheck prop_ordenadosPorMaximo

-- La comprobación es
--    λ> verifica_ordenadosPorMaximo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (ordenadosPorMaximo1 [[1..k] | k <- [1..10^4]])
--    10000
--    (6.00 secs, 8,763,714,848 bytes)
--    λ> length (ordenadosPorMaximo2 [[1..k] | k <- [1..10^4]])
--    10000
--    (6.15 secs, 8,764,177,472 bytes)
--    λ> length (ordenadosPorMaximo3 [[1..k] | k <- [1..10^4]])
--    10000
--    (8.16 secs, 13,914,503,672 bytes)
--    λ> length (ordenadosPorMaximo4 [[1..k] | k <- [1..10^4]])
--    10000
--    (7.77 secs, 13,914,183,776 bytes)
