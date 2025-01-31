-- Ordenados_por_maximo.hs
-- Ordenación por el máximo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-enero-2025
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Ordenados_por_maximo where

import Data.List (sort, sortBy)
import GHC.Exts (sortWith)
import Data.Map (elems, fromList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

ordenadosPorMaximo1 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo1 xss =
  map snd (sort [((maximum xs,k),xs) | (k,xs) <- zip [0..] xss])

-- 2ª solución
-- ===========

ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
  [xs | (_,xs) <- sort [((maximum xs,k),xs) | (k,xs) <- zip [0..] xss]]

-- 3ª solución
-- ===========

ordenadosPorMaximo3 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo3 =
  sortBy (\xs ys -> compare (maximum xs) (maximum ys))

-- 4ª solución
-- ===========

ordenadosPorMaximo4 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo4 = sortWith maximum

-- 4ª solución
-- ===========

ordenadosPorMaximo5 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo5 xss =
  elems (fromList [((maximum xs, k), xs) | (k, xs) <- zip [0..] xss])

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG ordenadosPorMaximo = do
  it "e1" $
    ordenadosPorMaximo [[0,8],[9],[8,1],[6,3],[8,2],[6,1],[6,2]] `shouldBe`
      [[6,3],[6,1],[6,2],[0,8],[8,1],[8,2],[9]]

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadosPorMaximo1
  describe "def. 2" $ specG ordenadosPorMaximo2
  describe "def. 3" $ specG ordenadosPorMaximo3
  describe "def. 4" $ specG ordenadosPorMaximo4
  describe "def. 5" $ specG ordenadosPorMaximo5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_ordenadosPorMaximo :: [[Int]] -> Bool
prop_ordenadosPorMaximo xss =
  all (== ordenadosPorMaximo1 yss)
      [ordenadosPorMaximo2 yss,
       ordenadosPorMaximo3 yss,
       ordenadosPorMaximo4 yss,
       ordenadosPorMaximo5 yss]
  where yss = filter (not . null) xss

-- La comprobación es
--    λ> quickCheck prop_ordenadosPorMaximo
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
--    λ> length (ordenadosPorMaximo5 [[1..k] | k <- [1..10^4]])
--    10000
--    (6.71 secs, 3,607,840,248 bytes)
