-- Igualdad_de_conjuntos.hs
-- Igualdad de conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    iguales :: Ord a => [a] -> [a] -> Bool
-- tal que (iguales xs ys) se verifica si xs e ys son iguales. Por
-- ejemplo,
--    iguales [3,2,3] [2,3]    ==  True
--    iguales [3,2,3] [2,3,2]  ==  True
--    iguales [3,2,3] [2,3,4]  ==  False
--    iguales [2,3] [4,5]      ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Igualdad_de_conjuntos where

import Data.List (nub, sort)
import Data.Set (fromList)
import Test.QuickCheck

-- 1ª solución
-- ===========

iguales1 :: Ord a => [a] -> [a] -> Bool
iguales1 xs ys =
  subconjunto xs ys && subconjunto ys xs

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo,
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
subconjunto :: Ord a => [a] -> [a] -> Bool
subconjunto xs ys =
  [x | x <- xs, x `elem` ys] == xs

-- 2ª solución
-- ===========

iguales2 :: Ord a => [a] -> [a] -> Bool
iguales2 xs ys =
  nub (sort xs) == nub (sort ys)

-- 3ª solución
-- ===========

iguales3 :: Ord a => [a] -> [a] -> Bool
iguales3 xs ys =
  fromList xs == fromList ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_iguales :: [Int] -> [Int] -> Bool
prop_iguales xs ys =
  all (== iguales1 xs ys)
      [iguales2 xs ys,
       iguales3 xs ys]

-- La comprobación es
--    λ> quickCheck prop_iguales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> iguales1 [1..2*10^4] [1..2*10^4]
--    True
--    (4.05 secs, 8,553,104 bytes)
--    λ> iguales2 [1..2*10^4] [1..2*10^4]
--    True
--    (4.14 secs, 9,192,768 bytes)
--    λ> iguales3 [1..2*10^4] [1..2*10^4]
--    True
--    (0.01 secs, 8,552,232 bytes)
