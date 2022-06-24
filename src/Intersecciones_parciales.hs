-- Intersecciones_parciales.hs
-- Intersecciones parciales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función 
--    interseccionParcial :: Ord a => Int -> [[a]] -> [a]
-- tal que (interseccionParcial n xss) es la lista de los elementos que
-- pertenecen al menos a n conjuntos de xss. Por ejemplo,
--    interseccionParcial 1 [[3,4],[4,5,9],[5,4,7]]  == [3,4,5,9,7]
--    interseccionParcial 2 [[3,4],[4,5,9],[5,4,7]]  == [4,5]
--    interseccionParcial 3 [[3,4],[4,5,9],[5,4,7]]  == [4]
--    interseccionParcial 4 [[3,4],[4,5,9],[5,4,7]]  == []
-- ---------------------------------------------------------------------

module Intersecciones_parciales where

import Data.List (foldl', nub, union, sort)
import qualified Data.Set as S
import Test.QuickCheck

-- 1ª solución
-- ===========

interseccionParcial1 :: Ord a => Int -> [[a]] -> [a]
interseccionParcial1 n xss = 
  [x | x <- sort (elementos xss)
     , pertenecenAlMenos n xss x]

elementos :: Ord a => [[a]] -> [a]
elementos []       = []
elementos (xs:xss) = xs `union` elementos xss

pertenecenAlMenos :: Ord a => Int -> [[a]] -> a -> Bool
pertenecenAlMenos n xss x =
  length [xs | xs <- xss, x `elem` xs] >= n
  
-- 2ª solución
-- ===========

interseccionParcial2 :: Ord a => Int -> [[a]] -> [a]
interseccionParcial2 n xss = 
  [x | x <- sort (elementos2 xss)
     , pertenecenAlMenos2 n xss x]

elementos2 :: Ord a => [[a]] -> [a]
elementos2 = foldl' union []
  
pertenecenAlMenos2 :: Ord a => Int -> [[a]] -> a -> Bool
pertenecenAlMenos2 n xss x =
  length (filter (x `elem`) xss) >= n
  
-- 3ª solución
-- ===========

interseccionParcial3 :: Ord a => Int -> [[a]] -> [a]
interseccionParcial3 n xss = 
  [x | x <- sort (nub (concat xss))
     , length [xs | xs <- xss, x `elem` xs] >= n]

-- 4ª solución
-- ===========

-- interseccionParcial4 :: Ord a => Int -> [[a]] -> [a]
-- interseccionParcial4 n xss = 
--   toList (interseccionParcial4Aux n (fromList (map fromList xss)))
-- 
-- interseccionParcial4 :: Ord a => Int -> S.Set (S.Set a) -> S.Set a
-- interseccionParcial4 n xss = 
--   S.filter  elementos4 xss
--      , length [xs | xs <- xss, x `elem` xs] >= n]
-- 
-- elementos4 :: Ord a => [S.Set a] -> S.Set a
-- elementos4 = S.foldl' S.union S.empty
  
-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_interseccionParcial :: Positive Int -> [[Int]] -> Bool
prop_interseccionParcial (Positive n) xss =
  all (== interseccionParcial1 n yss)
      [interseccionParcial2 n yss,
       interseccionParcial3 n yss]
  where yss = map nub xss

-- La comprobación es
--    λ> quickCheck prop_interseccionParcial
--    +++ OK, passed 100 tests.
