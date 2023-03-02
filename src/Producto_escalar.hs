-- Producto_escalar.hs
-- Producto escalar
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El producto escalar de dos listas de enteros xs y ys de longitud n
-- viene dado por la suma de los productos de los elementos
-- correspondientes.
--
-- Definir la función
--    productoEscalar :: [Integer] -> [Integer] -> Integer
-- tal que (productoEscalar xs ys) es el producto escalar de las listas
-- xs e ys. Por ejemplo,
--    productoEscalar [1,2,3] [4,5,6]  ==  32
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Producto_escalar where

import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

productoEscalar1 :: [Integer] -> [Integer] -> Integer
productoEscalar1 xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 2ª solución
-- ===========

productoEscalar2 :: [Integer] -> [Integer] -> Integer
productoEscalar2 xs ys = sum (zipWith (*) xs ys)

-- 3ª solución
-- ===========

productoEscalar3 :: [Integer] -> [Integer] -> Integer
productoEscalar3 = (sum .) . zipWith (*)

-- 4ª solución
-- ===========

productoEscalar4 :: [Integer] -> [Integer] -> Integer
productoEscalar4 [] _          = 0
productoEscalar4 _ []          = 0
productoEscalar4 (x:xs) (y:ys) = x*y + productoEscalar4 xs ys

-- 5ª solución
-- ===========

productoEscalar5 :: [Integer] -> [Integer] -> Integer
productoEscalar5 (x:xs) (y:ys) = x*y + productoEscalar5 xs ys
productoEscalar5 _ _           = 0

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_productoEscalar :: [Integer] -> [Integer] -> Bool
prop_productoEscalar xs ys =
  all (== productoEscalar1 xs ys)
      [productoEscalar2 xs ys,
       productoEscalar3 xs ys,
       productoEscalar4 xs ys,
       productoEscalar5 xs ys]

-- La comprobación es
--    λ> quickCheck prop_productoEscalar
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> productoEscalar1 (replicate (2*10^6) 1) (replicate (2*10^6) 1)
--    2000000
--    (1.37 secs, 803,827,520 bytes)
--    λ> productoEscalar2 (replicate (2*10^6) 1) (replicate (2*10^6) 1)
--    2000000
--    (0.69 secs, 611,008,272 bytes)
--    λ> productoEscalar3 (replicate (2*10^6) 1) (replicate (2*10^6) 1)
--    2000000
--    (0.69 secs, 611,008,536 bytes)
--    λ> productoEscalar4 (replicate (2*10^6) 1) (replicate (2*10^6) 1)
--    2000000
--    (1.64 secs, 742,290,272 bytes)
--    λ> productoEscalar5 (replicate (2*10^6) 1) (replicate (2*10^6) 1)
--    2000000
--    (1.63 secs, 742,290,064 bytes)
--    λ> productoEscalar6 (replicate (2*10^6) 1) (replicate (2*10^6) 1)
--    2000000
--    (0.32 secs, 835,679,200 bytes)
--
--    λ> productoEscalar2 (replicate (6*10^6) 1) (replicate (6*10^6) 1)
--    6000000
--    (1.90 secs, 1,831,960,336 bytes)
--    λ> productoEscalar3 (replicate (6*10^6) 1) (replicate (6*10^6) 1)
--    6000000
--    (1.87 secs, 1,831,960,600 bytes)
--    λ> productoEscalar6 (replicate (6*10^6) 1) (replicate (6*10^6) 1)
--    6000000
--    (0.78 secs, 2,573,005,952 bytes)
