-- Interseccion_conjuntista_de_listas.hs
-- Intersección conjuntista de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    interseccion :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de las listas sin
-- elementos repetidos xs e ys. Por ejemplo,
--    interseccion [3,2,5] [5,7,3,4]  ==  [3,5]
--    interseccion [3,2,5] [9,7,6,4]  ==  []
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Interseccion_conjuntista_de_listas where

import Data.List (nub, sort, intersect)
import qualified Data.Set as S (fromList, toList, intersection )
import Test.QuickCheck

-- 1ª solución
-- ===========

interseccion1 :: Eq a => [a] -> [a] -> [a]
interseccion1 xs ys =
  [x | x <- xs, x `elem` ys]

-- 2ª solución
-- ===========

interseccion2 :: Ord a => [a] -> [a] -> [a]
interseccion2 [] _ = []
interseccion2 (x:xs) ys
  | x `elem` ys = x : xs `interseccion2` ys
  | otherwise   = xs `interseccion2` ys

-- 3ª solución
-- ===========

interseccion3 :: Ord a => [a] -> [a] -> [a]
interseccion3 = intersect

-- 4ª solución
-- ===========

interseccion4 :: Ord a => [a] -> [a] -> [a]
interseccion4 xs ys =
  S.toList (S.fromList xs `S.intersection` S.fromList ys)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_interseccion :: [Int] -> [Int] -> Bool
prop_interseccion xs ys =
  all (== sort (xs' `interseccion1` ys'))
      [sort (xs' `interseccion2` ys'),
       sort (xs' `interseccion3` ys'),
       xs' `interseccion4` ys']
  where xs' = nub xs
        ys' = nub ys

-- La comprobación es
--    λ> quickCheck prop_interseccion
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (interseccion1 [0..3*10^4] [1,3..3*10^4])
--    15000
--    (2.94 secs, 6,673,360 bytes)
--    λ> length (interseccion2 [0..3*10^4] [1,3..3*10^4])
--    15000
--    (3.04 secs, 9,793,440 bytes)
--    λ> length (interseccion3 [0..3*10^4] [1,3..3*10^4])
--    15000
--    (5.39 secs, 6,673,472 bytes)
--    λ> length (interseccion4 [0..3*10^4] [1,3..3*10^4])
--    15000
--    (0.04 secs, 8,593,176 bytes)
