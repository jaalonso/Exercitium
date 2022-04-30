-- Producto_cartesiano.hs
-- Producto cartesiano de una familia de conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos xss.
-- Por ejemplo,
--    λ> producto1 [[2,5],[6,4]]
--    [[2,6],[2,4],[5,6],[5,4]]
--    λ> producto1 [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    λ> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    λ> producto []
--    [[]]
--
-- Comprobar con QuickCheck que para toda lista de listas de números
-- enteros, xss, se verifica que el número de elementos de (producto
-- xss) es igual al producto de los números de elementos de cada una de
-- las listas de xss.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Producto_cartesiano where

import Test.QuickCheck (quickCheck)
import Control.Monad (liftM2)
import Control.Applicative (liftA2)

-- 1ª solución
-- ===========

producto1 :: [[a]] -> [[a]]
producto1 []       = [[]]
producto1 (xs:xss) = [x:ys | x <- xs, ys <- producto1 xss]

-- 2ª solución
-- ===========

producto2 :: [[a]] -> [[a]]
producto2 []       = [[]]
producto2 (xs:xss) = [x:ys | x <- xs, ys <- ps]
  where ps = producto2 xss

-- 3ª solución
-- ===========

producto3 :: [[a]] -> [[a]]
producto3 []       = [[]]
producto3 (xs:xss) = inserta3 xs (producto3 xss)

-- (inserta xs xss) inserta cada elemento de xs en los elementos de
-- xss. Por ejemplo,
--    λ> inserta [1,2] [[3,4],[5,6]]
--    [[1,3,4],[1,5,6],[2,3,4],[2,5,6]]
inserta3 :: [a] -> [[a]] -> [[a]]
inserta3 [] _       = []
inserta3 (x:xs) yss = [x:ys | ys <- yss] ++ inserta3 xs yss

-- 4ª solución
-- ===========

producto4 :: [[a]] -> [[a]]
producto4 = foldr inserta4 [[]]

inserta4 :: [a] -> [[a]] -> [[a]]
inserta4 []     _   = []
inserta4 (x:xs) yss = map (x:) yss ++ inserta4 xs yss

-- 5ª solución
-- ===========

producto5 :: [[a]] -> [[a]]
producto5 = foldr inserta5 [[]]

inserta5 :: [a] -> [[a]] -> [[a]]
inserta5 xs yss = [x:ys | x <- xs, ys <- yss]

-- 6ª solución
-- ===========

producto6 :: [[a]] -> [[a]]
producto6 = foldr inserta6 [[]]

inserta6 :: [a] -> [[a]] -> [[a]]
inserta6 xs yss = concatMap (\x -> map (x:) yss) xs

-- 7ª solución
-- ===========

producto7 :: [[a]] -> [[a]]
producto7 = foldr inserta7 [[]]

inserta7 :: [a] -> [[a]] -> [[a]]
inserta7 xs yss = xs >>= (\x -> map (x:) yss)

-- 8ª solución
-- ===========

producto8 :: [[a]] -> [[a]]
producto8 = foldr inserta8 [[]]

inserta8 :: [a] -> [[a]] -> [[a]]
inserta8 xs yss = (:) <$> xs <*> yss

-- 9ª solución
-- ===========

producto9 :: [[a]] -> [[a]]
producto9 = foldr inserta9 [[]]

inserta9 :: [a] -> [[a]] -> [[a]]
inserta9 = liftA2 (:)

-- 10ª solución
-- ============

producto10 :: [[a]] -> [[a]]
producto10 = foldr (liftM2 (:)) [[]]

-- 11ª solución
-- ============

producto11 :: [[a]] -> [[a]]
producto11 = sequence

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_producto :: [[Int]] -> Bool
prop_producto xss =
  all (== producto1 xss)
      [ producto2 xss
      , producto3 xss
      , producto4 xss
      , producto5 xss
      , producto6 xss
      , producto7 xss
      , producto8 xss
      , producto9 xss
      , producto10 xss
      , producto11 xss
      ]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize = 9}) prop_producto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (producto1 (replicate 7 [0..9]))
--    10000000
--    (10.51 secs, 10,169,418,496 bytes)
--    λ> length (producto2 (replicate 7 [0..9]))
--    10000000
--    (2.14 secs, 1,333,870,712 bytes)
--    λ> length (producto3 (replicate 7 [0..9]))
--    10000000
--    (3.33 secs, 1,956,102,056 bytes)
--    λ> length (producto4 (replicate 7 [0..9]))
--    10000000
--    (0.98 secs, 1,600,542,752 bytes)
--    λ> length (producto5 (replicate 7 [0..9]))
--    10000000
--    (2.10 secs, 1,333,870,288 bytes)
--    λ> length (producto6 (replicate 7 [0..9]))
--    10000000
--    (1.17 secs, 1,600,534,632 bytes)
--    λ> length (producto7 (replicate 7 [0..9]))
--    10000000
--    (0.35 secs, 1,600,534,352 bytes)
--    λ> length (producto8 (replicate 7 [0..9]))
--    10000000
--    (0.87 secs, 978,317,848 bytes)
--    λ> length (producto9 (replicate 7 [0..9]))
--    10000000
--    (1.38 secs, 1,067,201,016 bytes)
--    λ> length (producto10 (replicate 7 [0..9]))
--    10000000
--    (0.54 secs, 2,311,645,392 bytes)
--    λ> length (producto11 (replicate 7 [0..9]))
--    10000000
--    (1.32 secs, 1,067,200,992 bytes)
--
--    λ> length (producto7 (replicate 7 [1..14]))
--    105413504
--    (3.77 secs, 16,347,739,040 bytes)
--    λ> length (producto10 (replicate 7 [1..14]))
--    105413504
--    (5.11 secs, 23,613,162,016 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_longitud :: [[Int]] -> Bool
prop_longitud xss =
  length (producto7 xss) == product (map length xss)

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize = 7}) prop_longitud
--    +++ OK, passed 100 tests.
