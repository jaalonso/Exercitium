-- Segmentos_consecutivos.hs
-- Segmentos de elementos consecutivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    segmentos :: (Enum a, Eq a) => [a] -> [[a]]
-- tal que (segmentos xss) es la lista de los segmentos de xss formados
-- por elementos consecutivos. Por ejemplo,
--    segmentos [1,2,5,6,4]     ==  [[1,2],[5,6],[4]]
--    segmentos [1,2,3,4,7,8,9] ==  [[1,2,3,4],[7,8,9]]
--    segmentos "abbccddeeebc"  ==  ["ab","bc","cd","de","e","e","bc"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Segmentos_consecutivos where

import Test.QuickCheck

-- 1ª solución
-- ===========

segmentos1 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos1 []  = []
segmentos1 [x] = [[x]]
segmentos1 (x:xs) | y == succ x = (x:y:ys):zs
                  | otherwise   = [x] : (y:ys):zs
  where ((y:ys):zs) = segmentos1 xs

-- 2ª solución
-- ===========

segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
  where ys = inicial xs
        n  = length ys
        zs = drop n xs

-- (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
--    inicial [1,2,5,6,4]    ==  [1,2]
--    inicial "abccddeeebc"  ==  "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial [] = []
inicial (x:xs) =
  [y | (y,_) <- takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..])]

-- 3ª solución
-- ===========

segmentos3 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos3 []  = []
segmentos3 xs = ys : segmentos3 zs
  where ys = inicial3 xs
        n  = length ys
        zs = drop n xs

inicial3 :: (Enum a, Eq a) => [a] -> [a]
inicial3 [] = []
inicial3 (x:xs) =
  [y | (y,_) <- takeWhile (uncurry (==)) (zip (x:xs) [x..])]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_segmentos :: [Int] -> Bool
prop_segmentos xs =
  all (== segmentos1 xs)
      [segmentos2 xs,
       segmentos3 xs]

-- La comprobación es
--    λ> quickCheck prop_segmentos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> xs = show (7^(10^6))
--    λ> length (segmentos1 xs)
--    768710
--    (1.94 secs, 823,156,768 bytes)
--    λ> length (segmentos2 xs)
--    768710
--    (1.66 secs, 960,884,224 bytes)
--    λ> length (segmentos3 xs)
--    768710
--    (1.49 secs, 1,009,470,560 bytes)
