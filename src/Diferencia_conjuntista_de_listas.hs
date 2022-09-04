-- Diferencia_conjuntista_de_listas.hs
-- Diferencia conjuntista de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia de las listas sin
-- elementos repetidos xs e ys. Por ejemplo,
--    diferencia [3,2,5,6] [5,7,3,4]  ==  [2,6]
--    diferencia [3,2,5] [5,7,3,2]    ==  []
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Diferencia_conjuntista_de_listas where

import Data.List (nub, sort, (\\))
import qualified Data.Set as S (fromList, toList, (\\) )
import Test.QuickCheck

-- 1ª solución
-- ===========

diferencia1 :: Eq a => [a] -> [a] -> [a]
diferencia1 xs ys =
  [x | x <- xs, x `notElem` ys]

-- 2ª solución
-- ===========

diferencia2 :: Ord a => [a] -> [a] -> [a]
diferencia2 [] _ = []
diferencia2 (x:xs) ys
  | x `elem` ys = xs `diferencia2` ys
  | otherwise   = x : xs `diferencia2` ys

-- 3ª solución
-- ===========

diferencia3 :: Ord a => [a] -> [a] -> [a]
diferencia3 = (\\)

-- 4ª solución
-- ===========

diferencia4 :: Ord a => [a] -> [a] -> [a]
diferencia4 xs ys =
  S.toList (S.fromList xs S.\\ S.fromList ys)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_diferencia :: [Int] -> [Int] -> Bool
prop_diferencia xs ys =
  all (== sort (xs' `diferencia1` ys'))
      [sort (xs' `diferencia2` ys'),
       sort (xs' `diferencia3` ys'),
       xs' `diferencia4` ys']
  where xs' = nub xs
        ys' = nub ys

-- La comprobación es
--    λ> quickCheck prop_diferencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (diferencia1 [0..3*10^4] [1,3..3*10^4])
--    15001
--    (3.39 secs, 9,553,528 bytes)
--    λ> length (diferencia2 [0..3*10^4] [1,3..3*10^4])
--    15001
--    (2.98 secs, 9,793,528 bytes)
--    λ> length (diferencia3 [0..3*10^4] [1,3..3*10^4])
--    15001
--    (3.61 secs, 11,622,502,792 bytes)
--    λ> length (diferencia4 [0..3*10^4] [1,3..3*10^4])
--    15001
--    (0.02 secs, 10,092,832 bytes)
