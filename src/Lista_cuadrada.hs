-- Lista_cuadrada.hs
-- Lista cuadrada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    listaCuadrada :: Int -> a -> [a] -> [[a]]
-- tal que (listaCuadrada n x xs) es una lista de n listas de longitud n
-- formadas con los elementos de xs completada con x, si no xs no tiene
-- suficientes elementos. Por ejemplo,
--    listaCuadrada 3 7 [0,3,5,2,4]  ==  [[0,3,5],[2,4,7],[7,7,7]]
--    listaCuadrada 3 7 [0..]        ==  [[0,1,2],[3,4,5],[6,7,8]]
--    listaCuadrada 2 'p' "eva"      ==  ["ev","ap"]
--    listaCuadrada 2 'p' ['a'..]    ==  ["ab","cd"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lista_cuadrada where

import Data.List.Split (chunksOf)
import Test.QuickCheck

-- 1ª solución
-- ===========

listaCuadrada1 :: Int -> a -> [a] -> [[a]]
listaCuadrada1 n x xs =
  take n (grupos n (xs ++ repeat x))

-- (grupos n xs) es la lista obtenida agrupando los elementos de xs en
-- grupos de n elementos, salvo el último que puede tener menos. Por
-- ejemplo,
--    grupos 2 [4,2,5,7,6]     ==  [[4,2],[5,7],[6]]
--    take 3 (grupos 3 [1..])  ==  [[1,2,3],[4,5,6],[7,8,9]]
grupos :: Int -> [a] -> [[a]]
grupos _ [] = []
grupos n xs = take n xs : grupos n (drop n xs)

-- 2ª solución
-- ===========

listaCuadrada2 :: Int -> a -> [a] -> [[a]]
listaCuadrada2 n x xs =
  take n (grupos2 n (xs ++ repeat x))

grupos2 :: Int -> [a] -> [[a]]
grupos2 _ [] = []
grupos2 n xs = ys : grupos n zs
  where (ys,zs) = splitAt n xs

-- 3ª solución
-- ===========

listaCuadrada3 :: Int -> a -> [a] -> [[a]]
listaCuadrada3 n x xs =
  take n (chunksOf n (xs ++ repeat x))

-- Comprobación de la equivalencia
-- ===============================

-- La propiedad es
prop_listaCuadrada :: Int -> Int -> [Int] -> Bool
prop_listaCuadrada n x xs =
  all (== listaCuadrada1 n x xs)
      [listaCuadrada2 n x xs,
       listaCuadrada3 n x xs]

-- La comprobación es
--    λ> quickCheck prop_listaCuadrada
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (listaCuadrada1 (10^4) 5 [1..])
--    10000
--    (2.02 secs, 12,801,918,616 bytes)
--    λ> length (listaCuadrada2 (10^4) 5 [1..])
--    10000
--    (1.89 secs, 12,803,198,576 bytes)
--    λ> length (listaCuadrada3 (10^4) 5 [1..])
--    10000
--    (1.85 secs, 12,801,518,728 bytes)
