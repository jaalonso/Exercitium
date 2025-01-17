-- Elementos_minimales.hs
-- Determinación de los elementos minimales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-enero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    minimales :: Ord a => [[a]] -> [[a]]
-- tal que (minimales xss) es la lista de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
--    minimales [[1,3],[2,3,1],[3,2,5]]        ==  [[2,3,1],[3,2,5]]
--    minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  ==  [[2,3,1],[3,2,5]]
--    map sum (minimales [[1..n] | n <- [1..300]])  ==  [45150]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elementos_minimales where

import Data.List (delete, nub)
import Data.Set (fromList, isProperSubsetOf)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

minimales1 :: Ord a => [[a]] -> [[a]]
minimales1 xss =
  [xs | xs <- xss,
        null [ys | ys <- xss, subconjuntoPropio xs ys]]

-- (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo,
--    subconjuntoPropio [1,3] [3,1,3]    ==  False
--    subconjuntoPropio [1,3,1] [3,1,2]  ==  True
subconjuntoPropio :: Ord a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = aux (nub xs) (nub ys)
  where
    aux _       []  = False
    aux []      _   = True
    aux (u:us) vs = u `elem` vs && aux us (delete u vs)

-- 2ª solución
-- ===========

minimales2 :: Ord a => [[a]] -> [[a]]
minimales2 xss =
  [xs | xs <- xss,
        null [ys | ys <- xss, subconjuntoPropio2 xs ys]]

subconjuntoPropio2 :: Ord a => [a] -> [a] -> Bool
subconjuntoPropio2 xs ys =
  subconjunto xs ys && not (subconjunto ys xs)

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo,
--    subconjunto [1,3] [3,1,3]        ==  True
--    subconjunto [1,3,1,3] [3,1,3]    ==  True
--    subconjunto [1,3,2,3] [3,1,3]    ==  False
--    subconjunto [1,3,1,3] [3,1,3,2]  ==  True
subconjunto :: Ord a => [a] -> [a] -> Bool
subconjunto xs ys =
  all (`elem` ys) xs

-- 3ª solución
-- ===========

minimales3 :: Ord a => [[a]] -> [[a]]
minimales3 xss =
  [xs | xs <- xss,
        null [ys | ys <- xss, subconjuntoPropio3 xs ys]]

subconjuntoPropio3 :: Ord a => [a] -> [a] -> Bool
subconjuntoPropio3 xs ys =
  isProperSubsetOf (fromList xs) (fromList ys)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG minimales = do
  it "e1" $
    minimales [[1,3],[2,3,1],[3,2,5]]        `shouldBe`  [[2,3,1],[3,2,5]]
  it "e2" $
    minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  `shouldBe`  [[2,3,1],[3,2,5]]

spec :: Spec
spec = do
  describe "def. 1" $ specG minimales1
  describe "def. 2" $ specG minimales2
  describe "def. 3" $ specG minimales3

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_minimales :: [[Int]] -> Bool
prop_minimales xss =
  all (== minimales1 xss)
      [minimales2 xss,
       minimales3 xss]

-- La comprobación es
--    λ> quickCheck prop_minimales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (minimales1 [[1..n] | n <- [1..200]])
--    1
--    (2.07 secs, 702,257,168 bytes)
--    λ> length (minimales2 [[1..n] | n <- [1..200]])
--    1
--    (0.85 secs, 138,498,288 bytes)
--    λ> length (minimales3 [[1..n] | n <- [1..200]])
--    1
--    (0.15 secs, 293,134,464 bytes)
--
--    λ> length (minimales2 [[1..n] | n <- [1..300]])
--    1
--    (3.61 secs, 442,569,888 bytes)
--    λ> length (minimales3 [[1..n] | n <- [1..300]])
--    1
--    (0.39 secs, 981,990,968 bytes)
