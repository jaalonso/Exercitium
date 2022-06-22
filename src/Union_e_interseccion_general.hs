-- Union_e_interseccion_general.hs
-- Unión e intersección general de conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir las funciones
--    unionGeneral        :: Eq a => [[a]] -> [a]
--    interseccionGeneral :: Eq a => [[a]] -> [a]
-- tales que 
-- + (unionGeneral xs) es la unión de los conjuntos de la lista de
--   conjuntos xs (es decir, el conjunto de los elementos que pertenecen
--   a alguno de los elementos de xs). Por ejemplo,
--      unionGeneral []                    ==  []
--      unionGeneral [[1]]                 ==  [1]
--      unionGeneral [[1],[1,2],[2,3]]     ==  [1,2,3]
--      unionGeneral ([[x] | x <- [1..9]]) ==  [1,2,3,4,5,6,7,8,9]
-- + (interseccionGeneral xs) es la intersección de los conjuntos de la
--   lista de conjuntos xs (es decir, el conjunto de los elementos que
--   pertenecen a todos los elementos de xs). Por ejemplo,
--      interseccionGeneral [[1]]                      ==  [1]
--      interseccionGeneral [[2],[1,2],[2,3]]          ==  [2]
--      interseccionGeneral [[2,7,5],[1,5,2],[5,2,3]]  ==  [2,5]
--      interseccionGeneral ([[x] | x <- [1..9]])      ==  []
--      interseccionGeneral (replicate (10^6) [1..5])  ==  [1,2,3,4,5]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Union_e_interseccion_general where

import Data.List (foldl', foldl1', intersect, nub, union)
import Test.QuickCheck (NonEmptyList (NonEmpty), quickCheck)

-- 1ª definición de unionGeneral
-- =============================

unionGeneral1 :: Eq a => [[a]] -> [a]
unionGeneral1 []     = []
unionGeneral1 (x:xs) = x `union` unionGeneral1 xs 

-- 2ª definición de unionGeneral
-- =============================

unionGeneral2 :: Eq a => [[a]] -> [a]
unionGeneral2 = foldr union []

-- 3ª definición de unionGeneral
-- =============================

unionGeneral3 :: Eq a => [[a]] -> [a]
unionGeneral3 = foldl' union []

-- Comprobación de equivalencia de unionGeneral
-- ============================================

-- La propiedad es
prop_unionGeneral :: [[Int]] -> Bool
prop_unionGeneral xss =
  all (== unionGeneral1 xss')
      [unionGeneral2 xss',
       unionGeneral3 xss']
  where xss' = nub (map nub xss)
  
-- La comprobación es
--    λ> quickCheck prop_unionGeneral
--    +++ OK, passed 100 tests.
--    (0.85 secs, 1,017,807,600 bytes)

-- Comparación de eficiencia de unionGeneral
-- =========================================

-- La comparación es
--    λ> length (unionGeneral1 ([[x] | x <- [1..10^3]]))
--    1000
--    (1.56 secs, 107,478,456 bytes)
--    λ> length (unionGeneral2 ([[x] | x <- [1..10^3]]))
--    1000
--    (1.50 secs, 107,406,560 bytes)
--    λ> length (unionGeneral3 ([[x] | x <- [1..10^3]]))
--    1000
--    (0.07 secs, 92,874,024 bytes)

-- 1ª definición de interseccionGeneral
-- ====================================

interseccionGeneral1 :: Eq a => [[a]] -> [a]
interseccionGeneral1 [x]    = x
interseccionGeneral1 (x:xs) = x `intersect` interseccionGeneral1 xs 

-- 2ª definición de interseccionGeneral
-- ====================================

interseccionGeneral2 :: Eq a => [[a]] -> [a]
interseccionGeneral2 = foldr1 intersect

-- 3ª definición de interseccionGeneral
-- ====================================

interseccionGeneral3 :: Eq a => [[a]] -> [a]
interseccionGeneral3 = foldl1' intersect

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_interseccionGeneral :: NonEmptyList [Int] -> Bool
prop_interseccionGeneral (NonEmpty xss) =
  all (== interseccionGeneral1 xss')
      [interseccionGeneral2 xss',
       interseccionGeneral3 xss']
  where xss' = nub (map nub xss)

-- La comprobación es
--    λ> quickCheck prop_interseccionGeneral
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> interseccionGeneral1 (replicate (10^6) [1..5])
--    [1,2,3,4,5]
--    (2.02 secs, 1,173,618,400 bytes)
--    λ> interseccionGeneral2 (replicate (10^6) [1..5])
--    [1,2,3,4,5]
--    (1.83 secs, 1,092,120,224 bytes)
--    λ> interseccionGeneral3 (replicate (10^6) [1..5])
--    [1,2,3,4,5]
--    (1.33 secs, 985,896,136 bytes)
