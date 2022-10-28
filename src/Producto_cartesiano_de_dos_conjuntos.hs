-- Producto_cartesiano_de_dos_conjuntos.hs
-- Producto cartesiano de dos conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    producto :: [a] -> [b] -> [(a,b)]
-- tal que (producto xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo,
--    producto [1,3] [2,4] == [(1,2),(1,4),(3,2),(3,4)]
--
-- Comprobar con QuickCheck que el número de elementos de (producto xs
-- ys) es el producto del número de elementos de xs y de ys.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Producto_cartesiano_de_dos_conjuntos where

import Test.QuickCheck

-- 1ª solución
-- ===========

producto1 :: [a] -> [a] -> [(a,a)]
producto1 xs ys = [(x,y) | x <- xs, y <- ys]

-- 2ª solución
-- ===========

producto2 :: [a] -> [a] -> [(a,a)]
producto2 []     _  = []
producto2 (x:xs) ys = [(x,y) | y <- ys] ++ producto2 xs ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_producto :: [Int] -> [Int] -> Bool
prop_producto xs ys =
  producto1 xs ys `iguales` producto2 xs ys

-- (iguales xs ys) se verifica si xs e ys son iguales. Por ejemplo,
--    iguales [3,2,3] [2,3]    ==  True
--    iguales [3,2,3] [2,3,2]  ==  True
--    iguales [3,2,3] [2,3,4]  ==  False
--    iguales [2,3] [4,5]      ==  False
iguales :: Ord a => [a] -> [a] -> Bool
iguales xs ys =
  subconjunto xs ys && subconjunto ys xs

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. por
-- ejemplo,
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
subconjunto :: Ord a => [a] -> [a] -> Bool
subconjunto xs ys =
  [x | x <- xs, x `elem` ys] == xs

-- La comprobación es
--    λ> quickCheck prop_producto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (producto1 [1..4000] [1..4000])
--    16000000
--    (2.33 secs, 1,537,551,208 bytes)
--    λ> length (producto2 [1..4000] [1..4000])
--    16000000
--    (2.87 secs, 2,434,095,160 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_elementos_producto :: [Int] -> [Int] -> Bool
prop_elementos_producto xs ys =
  length (producto1 xs ys) == length xs * length ys

-- La comprobación es
--    λ> quickCheck prop_elementos_producto
--    +++ OK, passed 100 tests.
