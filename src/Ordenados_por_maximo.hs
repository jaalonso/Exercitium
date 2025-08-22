-- Ordenados_por_maximo.hs
-- Ordenación por el máximo.
-- José A. Alonso Jiménez https://jaalonso.github.io
-- Sevilla, 22-abril-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
-- tal que (ordenadosPorMaximo xss) es la lista de los elementos de xss
-- ordenada por sus máximos. Por ejemplo,
--    λ> ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
--    [[3,2],[1,4],[6,7,5]]
--    λ> ordenadosPorMaximo [[1],[0,1]]
--    [[1],[0,1]]
--    λ> ordenadosPorMaximo [[0,1],[1]]
--    [[0,1],[1]]
--    λ> ordenadosPorMaximo ["este","es","el","primero"]
--    ["el","primero","es","este"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Ordenados_por_maximo where

import Data.List (sort, sortOn, sortBy)
import Data.Ord (comparing)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

ordenadosPorMaximo1 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo1 xss =
  [ys | (_,_,ys) <- sort [(maximum xs,n,xs) | (n,xs) <- zip [0..] xss]]

-- 2ª solución
-- ===========

ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
  map (\(_,_,ys) -> ys) (sort [(maximum xs,n,xs) | (n,xs) <- zip [0..] xss])

-- 3ª solución
-- ===========

ordenadosPorMaximo3 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo3 = sortBy (comparing maximum)

-- 4ª solución
-- ===========

ordenadosPorMaximo4 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo4 = sortOn maximum

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG ordenadosPorMaximo = do
  it "e1" $
    ordenadosPorMaximo [[3,2],[6,7,5],[1,4]] `shouldBe`
      [[3,2],[1,4],[6,7,5]]
  it "e2" $
    ordenadosPorMaximo [[1],[0,1]] `shouldBe`
      [[1],[0,1]]
  it "e3" $
    ordenadosPorMaximo [[0,1],[1]] `shouldBe`
      [[0,1],[1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadosPorMaximo1
  describe "def. 2" $ specG ordenadosPorMaximo2
  describe "def. 3" $ specG ordenadosPorMaximo3
  describe "def. 4" $ specG ordenadosPorMaximo4

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_ordenadosPorMaximo :: [[Int]] -> Bool
prop_ordenadosPorMaximo xss =
  all (== ordenadosPorMaximo1 yss)
      [ordenadosPorMaximo2 yss,
       ordenadosPorMaximo3 yss,
       ordenadosPorMaximo4 yss]
  where yss = filter (not .null) xss

-- La comprobación es
--    λ> quickCheck prop_ordenadosPorMaximo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (ordenadosPorMaximo1 [[1..n] | n <- [1..10^4]])
--    10000
--    (6.52 secs, 3,607,440,800 bytes)
--    λ> length (ordenadosPorMaximo2 [[1..n] | n <- [1..10^4]])
--    10000
--    (7.30 secs, 3,607,600,776 bytes)
--    λ> length (ordenadosPorMaximo3 [[1..n] | n <- [1..10^4]])
--    10000
--    (7.53 secs, 3,604,559,928 bytes)
--    λ> length (ordenadosPorMaximo4 [[1..n] | n <- [1..10^4]])
--    10000
--    (7.09 secs, 3,606,159,952 bytes)
