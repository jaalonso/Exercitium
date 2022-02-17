-- Elementos_minimales.hs
-- Determinación de los elementos minimales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-febrero-2022)
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

module Elementos_minimales where

import Data.List (delete, nub)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

minimales :: Ord a => [[a]] -> [[a]]
minimales xss =
  [xs | xs <- xss,
        null [ys | ys <- xss, subconjuntoPropio xs ys]]

-- (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo,
--    subconjuntoPropio [1,3] [3,1,3]    ==  False
--    subconjuntoPropio [1,3,1] [3,1,2]  ==  True
subconjuntoPropio :: Ord a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = subconjuntoPropio' (nub xs) (nub ys)
  where
    subconjuntoPropio' _       [] = False
    subconjuntoPropio' []      _   = True
    subconjuntoPropio' (x:xs') ys' =
      x `elem` ys' && subconjuntoPropio xs' (delete x ys')

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

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_minimales :: [[Int]] -> Bool
prop_minimales xss =
   minimales xss == minimales2 xss

verifica_minimales :: IO ()
verifica_minimales =
  quickCheck prop_minimales

-- La comprobación es
--    λ> verifica_minimales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (minimales [[1..n] | n <- [1..100]])
--    1
--    (2.32 secs, 967,128,840 bytes)
--    λ> length (minimales2 [[1..n] | n <- [1..100]])
--    1
--    (0.13 secs, 14,731,872 bytes)
