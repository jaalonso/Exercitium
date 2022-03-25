-- Mas_repetido.hs
-- Elemento más repetido de manera consecutiva.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    masRepetido :: Ord a => [a] -> (a,Int)
-- tal que (masRepetido xs) es el elemento de xs que aparece más veces
-- de manera consecutiva en la lista junto con el número de sus
-- apariciones consecutivas; en caso de empate, se devuelve el mayor de
-- dichos elementos. Por ejemplo,
--    masRepetido [1,1,4,4,1]  ==  (4,2)
--    masRepetido [4,4,1,1,5]  ==  (4,2)
--    masRepetido "aadda"      ==  ('d',2)
--    masRepetido "ddaab"      ==  ('d',2)
--    masRepetido (show (product [1..5*10^4]))  ==  ('0',12499)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mas_repetido where

import Data.List (group)
import Data.Tuple (swap)
import Control.Arrow ((&&&))
import Test.QuickCheck

-- 1ª solución
-- ===========

masRepetido1 :: Ord a => [a] -> (a,Int)
masRepetido1 [x] = (x,1)
masRepetido1 (x:y:zs) | m > n      = (x,m)
                      | m == n     = (max x u,m)
                      | otherwise  = (u,n)
  where (u,n)  = masRepetido1 (y:zs)
        m      = length (takeWhile (==x) (x:y:zs))

-- 2ª solución
-- ===========

masRepetido2 :: Ord a => [a] -> (a,Int)
masRepetido2 xs = (n,z)
  where (z,n) = maximum [(1 + length ys,y) | (y:ys) <- group xs]

-- 3ª solución
-- ============

masRepetido3 :: Ord a => [a] -> (a,Int)
masRepetido3 xs =
  swap (maximum [(1 + length ys,y) | (y:ys) <- group xs])

-- 4ª solución
-- ============

masRepetido4 :: Ord a => [a] -> (a,Int)
masRepetido4 xs =
  swap (maximum (map (\ys -> (length ys, head ys)) (group xs)))

-- 5ª solución
-- ============

masRepetido5 :: Ord a => [a] -> (a,Int)
masRepetido5 =
  swap . maximum . map ((,) <$> length <*> head) . group

-- 6ª solución
-- ============

masRepetido6 :: Ord a => [a] -> (a,Int)
masRepetido6 =
  swap . maximum . map (length &&& head) . group

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_masRepetido :: NonEmptyList Int -> Bool
prop_masRepetido (NonEmpty xs) =
  all (== masRepetido1 xs)
      [masRepetido2 xs,
       masRepetido3 xs,
       masRepetido4 xs,
       masRepetido5 xs,
       masRepetido6 xs]

-- La comprobación es
--    λ> quickCheck prop_masRepetido
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> masRepetido1 (show (product [1..3*10^4]))
--    ('0',7498)
--    (3.72 secs, 2,589,930,952 bytes)
--    λ> masRepetido2 (show (product [1..3*10^4]))
--    ('0',7498)
--    (0.85 secs, 945,399,976 bytes)
--    λ> masRepetido3 (show (product [1..3*10^4]))
--    ('0',7498)
--    (0.86 secs, 945,399,888 bytes)
--    λ> masRepetido4 (show (product [1..3*10^4]))
--    ('0',7498)
--    (0.80 secs, 943,760,760 bytes)
--    λ> masRepetido5 (show (product [1..3*10^4]))
--    ('0',7498)
--    (0.78 secs, 945,400,400 bytes)
--    λ> masRepetido6 (show (product [1..3*10^4]))
--    ('0',7498)
--    (0.78 secs, 942,122,088 bytes)
--
--    λ> masRepetido2 (show (product [1..5*10^4]))
--    ('0',12499)
--    (2.20 secs, 2,716,952,408 bytes)
--    λ> masRepetido3 (show (product [1..5*10^4]))
--    ('0',12499)
--    (2.22 secs, 2,716,952,320 bytes)
--    λ> masRepetido4 (show (product [1..5*10^4]))
--    ('0',12499)
--    (2.18 secs, 2,714,062,328 bytes)
--    λ> masRepetido5 (show (product [1..5*10^4]))
--    ('0',12499)
--    (2.17 secs, 2,716,952,832 bytes)
--    λ> masRepetido6 (show (product [1..5*10^4]))
--    ('0',12499)
--    (2.17 secs, 2,711,172,792 bytes)
