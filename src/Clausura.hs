-- Clausura.hs
-- Clausura de un conjunto respecto de una función
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un conjunto A está cerrado respecto de una función  f si para todo
-- elemento x de A se tiene que f(x) pertenece a A. La clausura de un
-- conjunto B respecto de una función f es el menor conjunto A que
-- contiene a B y es cerrado respecto de f. Por ejemplo, la clausura de
-- {0,1,2] respecto del opuesto es {0,1,2,-1,-2}.
--
-- Definir la función
--    clausura :: Ord a => (a -> a) -> [a] -> [a]
-- tal que (clausura f xs) es la clausura ordenada de xs respecto de
-- f. Por ejemplo,
--    clausura (\x -> -x) [0,1,2]         ==  [-2,-1,0,1,2]
--    clausura (\x -> (x+1) `mod` 5) [0]  ==  [0,1,2,3,4]
--    length (clausura (\x -> (x+1) `mod` (10^6)) [0]) == 1000000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Clausura where

import Data.List ((\\), nub, sort, union)
import Test.QuickCheck.HigherOrder (quickCheck')
import qualified Data.Set as S (Set, difference, fromList, map, null, toList, union)

-- 1ª solución
-- ===========

clausura1 :: Ord a => (a -> a) -> [a] -> [a]
clausura1 f xs
  | esCerrado f xs = sort xs
  | otherwise      = clausura1 f (expansion f xs)

-- (esCerrado f xs) se verifica si al aplicar f a cualquier elemento de
-- xs se obtiene un elemento de xs. Por ejemplo,
--    λ> esCerrado (\x -> -x) [0,1,2]
--    False
--    λ> esCerrado (\x -> -x) [0,1,2,-2,-1]
--    True
esCerrado :: Ord a => (a -> a) -> [a] -> Bool
esCerrado f xs = all (`elem` xs) (map f xs)

-- (expansion f xs) es la lista (sin repeticiones) obtenidas añadiéndole
-- a xs el resulta de aplicar f a sus elementos. Por ejemplo,
--    expansion (\x -> -x) [0,1,2]  ==  [0,1,2,-1,-2]
expansion :: Ord a => (a -> a) -> [a] -> [a]
expansion f xs = xs `union` map f xs

-- 2ª solución
-- ===========

clausura2 :: Ord a => (a -> a) -> [a] -> [a]
clausura2 f xs = sort (until (esCerrado f) (expansion f) xs)

-- 3ª solución
-- ===========

clausura3 :: Ord a => (a -> a) -> [a] -> [a]
clausura3 f xs = aux xs xs
  where aux ys vs | null ns   = sort vs
                  | otherwise = aux ns (vs ++ ns)
          where ns = nub (map f ys) \\ vs

-- 4ª solución
-- ===========

clausura4 :: Ord a => (a -> a) -> [a] -> [a]
clausura4 f xs = S.toList (clausura4' f (S.fromList xs))

clausura4' :: Ord a => (a -> a) -> S.Set a -> S.Set a
clausura4' f xs = aux xs xs
  where aux ys vs | S.null ns = vs
                  | otherwise = aux ns (vs `S.union` ns)
          where ns = S.map f ys `S.difference` vs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_clausura :: (Int -> Int) -> [Int] -> Bool
prop_clausura f xs =
  all (== clausura1 f xs')
      [ clausura2 f xs'
      , clausura3 f xs'
      , clausura4 f xs'
      ]
  where xs' = sort (nub xs)

-- La comprobación es
--    λ> quickCheck' prop_clausura
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (clausura1 (\x -> (x+1) `mod` 800) [0])
--    800
--    (1.95 secs, 213,481,560 bytes)
--    λ> length (clausura2 (\x -> (x+1) `mod` 800) [0])
--    800
--    (1.96 secs, 213,372,824 bytes)
--    λ> length (clausura3 (\x -> (x+1) `mod` 800) [0])
--    800
--    (0.03 secs, 42,055,128 bytes)
--    λ> length (clausura4 (\x -> (x+1) `mod` 800) [0])
--    800
--    (0.01 secs, 1,779,768 bytes)
--
--    λ> length (clausura3 (\x -> (x+1) `mod` (10^4)) [0])
--    10000
--    (2.50 secs, 8,080,105,816 bytes)
--    λ> length (clausura4 (\x -> (x+1) `mod` (10^4)) [0])
--    10000
--    (0.05 secs, 27,186,920 bytes)
