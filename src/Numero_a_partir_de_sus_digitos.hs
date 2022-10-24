-- Numero_a_partir_de_sus_digitos.hs
-- Número a partir de sus dígitos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    listaNumero :: [Integer] -> Integer
-- tal que (listaNumero xs) es el número formado por los dígitos xs. Por
-- ejemplo,
--    listaNumero [5]        == 5
--    listaNumero [1,3,4,7]  == 1347
--    listaNumero [0,0,1]    == 1
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Numero_a_partir_de_sus_digitos where

import Data.List (foldl')
import Data.Digits (unDigits)
import Test.QuickCheck

-- 1ª solución
-- ===========

listaNumero1 :: [Integer] -> Integer
listaNumero1 = aux . reverse
  where
    aux :: [Integer] -> Integer
    aux []     = 0
    aux (x:xs) = x + 10 * aux xs

-- 2ª solución
-- ===========

listaNumero2 :: [Integer] -> Integer
listaNumero2 = aux 0
  where
    aux :: Integer -> [Integer] -> Integer
    aux r []     = r
    aux r (x:xs) = aux (x+10*r) xs

-- 3ª solución
-- ===========

listaNumero3 :: [Integer] -> Integer
listaNumero3 = aux 0
  where
    aux :: Integer -> [Integer] -> Integer
    aux = foldl (\ r x -> x + 10 * r)

-- 4ª solución
-- ===========

listaNumero4 :: [Integer] -> Integer
listaNumero4 = foldl' (\ r x -> x + 10 * r) 0

-- 5ª solución
-- ===========

listaNumero5 :: [Integer] -> Integer
listaNumero5 xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]

-- 6ª solución
-- ===========

listaNumero6 :: [Integer] -> Integer
listaNumero6 xs = sum (zipWith (\ y n -> y*10^n) (reverse xs) [0..])

-- 7ª solución
-- ===========

listaNumero7 :: [Integer] -> Integer
listaNumero7 = unDigits 10

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_listaNumero :: [Integer] -> Bool
prop_listaNumero xs =
  all (== listaNumero1 xs)
      [listaNumero2 xs,
       listaNumero3 xs,
       listaNumero4 xs,
       listaNumero5 xs,
       listaNumero6 xs,
       listaNumero7 xs]

-- La comprobación es
--    λ> quickCheck prop_listaNumero
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (listaNumero1 (replicate (10^5) 9)))
--    100000
--    (4.01 secs, 4,309,740,064 bytes)
--    λ> length (show (listaNumero2 (replicate (10^5) 9)))
--    100000
--    (4.04 secs, 4,307,268,856 bytes)
--    λ> length (show (listaNumero3 (replicate (10^5) 9)))
--    100000
--    (4.08 secs, 4,300,868,816 bytes)
--    λ> length (show (listaNumero4 (replicate (10^5) 9)))
--    100000
--    (0.42 secs, 4,288,480,208 bytes)
--    λ> length (show (listaNumero4 (replicate (10^5) 9)))
--    100000
--    (0.41 secs, 4,288,480,208 bytes)
--    λ> length (show (listaNumero5 (replicate (10^5) 9)))
--    100000
--    (43.35 secs, 10,702,827,328 bytes)
--    λ> length (show (listaNumero6 (replicate (10^5) 9)))
--    100000
--    (46.89 secs, 10,693,227,280 bytes)
--    λ> length (show (listaNumero7 (replicate (10^5) 9)))
--    100000
--    (4.33 secs, 4,297,499,344 bytes)
