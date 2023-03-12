-- Reconocimiento_de_subconjunto.hs
-- Reconocimiento de subconjunto.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 04-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    subconjunto :: Ord a => [a] -> [a] -> Bool
-- tal que (subconjunto xs ys) se verifica si xs es un subconjunto de
-- ys. por ejemplo,
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Reconocimiento_de_subconjunto where

import Data.List (nub, sort)
import Data.Set (fromList, isSubsetOf)
import Test.QuickCheck

-- 1ª solución
-- ===========

subconjunto1 :: Ord a => [a] -> [a] -> Bool
subconjunto1 xs ys =
  [x | x <- xs, x `elem` ys] == xs

-- 2ª solución
-- ===========

subconjunto2 :: Ord a => [a] -> [a] -> Bool
subconjunto2 []     _  = True
subconjunto2 (x:xs) ys = x `elem` ys && subconjunto2 xs ys

-- 3ª solución
-- ===========

subconjunto3 :: Ord a => [a] -> [a] -> Bool
subconjunto3 xs ys =
  all (`elem` ys) xs

-- 4ª solución
-- ===========

subconjunto4 :: Ord a => [a] -> [a] -> Bool
subconjunto4 xs ys =
  fromList xs `isSubsetOf` fromList ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_subconjunto :: [Int] -> [Int] -> Bool
prop_subconjunto xs ys =
  all (== subconjunto1 xs ys)
      [subconjunto2 xs ys,
       subconjunto3 xs ys,
       subconjunto4 xs ys]

-- La comprobación es
--    λ> quickCheck prop_subconjunto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> subconjunto1 [1..2*10^4] [1..2*10^4]
--    True
--    (1.81 secs, 5,992,448 bytes)
--    λ> subconjunto2 [1..2*10^4] [1..2*10^4]
--    True
--    (1.83 secs, 6,952,200 bytes)
--    λ> subconjunto3 [1..2*10^4] [1..2*10^4]
--    True
--    (1.75 secs, 4,712,304 bytes)
--    λ> subconjunto4 [1..2*10^4] [1..2*10^4]
--    True
--    (0.04 secs, 6,312,056 bytes)

-- En lo sucesivo, usaremos la 4ª definición
subconjunto :: Ord a => [a] -> [a] -> Bool
subconjunto = subconjunto4
