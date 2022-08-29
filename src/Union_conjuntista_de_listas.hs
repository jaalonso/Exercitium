-- Union_conjuntista_de_listas.hs
-- Unión conjuntista de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    union :: Ord a => [a] -> [a] -> [a]
-- tal que (union xs ys) es la unión de las listas sin elementos
-- repetidos xs e ys. Por ejemplo,
--    union [3,2,5] [5,7,3,4]  ==  [3,2,5,7,4]
--
-- Comprobar con QuickCheck que la unión es conmutativa.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Union_conjuntista_de_listas where

import Data.List (nub, sort, union)
import qualified Data.Set as S (fromList, toList, union)
import Test.QuickCheck

-- 1ª solución
-- ===========

union1 :: Ord a => [a] -> [a] -> [a]
union1 xs ys = xs ++ [y | y <- ys, y `notElem` xs]

-- 2ª solución
-- ===========

union2 :: Ord a => [a] -> [a] -> [a]
union2 [] ys = ys
union2 (x:xs) ys
  | x `elem` ys = xs `union2` ys
  | otherwise   = x : xs `union2` ys

-- 3ª solución
-- ===========

union3 :: Ord a => [a] -> [a] -> [a]
union3 = union

-- 4ª solución
-- ===========

union4 :: Ord a => [a] -> [a] -> [a]
union4 xs ys =
  S.toList (S.fromList xs `S.union` S.fromList ys)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys =
  all (== sort (xs' `union1` ys'))
      [sort (xs' `union2` ys'),
       sort (xs' `union3` ys'),
       xs' `union4` ys']
  where xs' = nub xs
        ys' = nub ys

-- La comprobación es
--    λ> quickCheck prop_union
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (union1 [0,2..3*10^4] [1,3..3*10^4])
--    30001
--    (2.37 secs, 7,153,536 bytes)
--    λ> length (union2 [0,2..3*10^4] [1,3..3*10^4])
--    30001
--    (2.38 secs, 6,553,752 bytes)
--    λ> length (union3 [0,2..3*10^4] [1,3..3*10^4])
--    30001
--    (11.56 secs, 23,253,553,472 bytes)
--    λ> length (union4 [0,2..3*10^4] [1,3..3*10^4])
--    30001
--    (0.04 secs, 10,992,056 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_union_conmutativa :: [Int] -> [Int] -> Bool
prop_union_conmutativa xs ys =
  iguales (xs `union1` ys) (ys `union1` xs)

-- (iguales xs ys) se verifica si xs e ys son iguales. Por ejemplo,
--    iguales [3,2,3] [2,3]    ==  True
--    iguales [3,2,3] [2,3,2]  ==  True
--    iguales [3,2,3] [2,3,4]  ==  False
--    iguales [2,3] [4,5]      ==  Falseiguales :: Ord a => [a] -> [a] -> Bool
iguales :: Ord a => [a] -> [a] -> Bool
iguales xs ys =
  S.fromList xs == S.fromList ys

-- La comprobación es
--    λ> quickCheck prop_union_conmutativa
--    +++ OK, passed 100 tests.
