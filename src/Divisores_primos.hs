-- Divisores_primos.hs
-- Divisores primos
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisoresPrimos :: Integer -> [Integer]
-- tal que (divisoresPrimos x) es la lista de los divisores primos de x.
-- Por ejemplo,
--    divisoresPrimos 40 == [2,5]
--    divisoresPrimos 70 == [2,5,7]w
--    length (divisoresPrimos (product [1..20000])) == 2262
-- ------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Divisores_primos where

import Data.List (nub)
import Data.Set (toList)
import Data.Numbers.Primes (isPrime, primeFactors)
import Math.NumberTheory.ArithmeticFunctions (divisors)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisoresPrimos1 :: Integer -> [Integer]
divisoresPrimos1 x = [n | n <- divisores1 x, primo1 n]

-- (divisores n) es la lista de los divisores del número n. Por ejemplo,
--    divisores 25  ==  [1,5,25]
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores1 :: Integer -> [Integer]
divisores1 n = [x | x <- [1..n], n `mod` x == 0]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True
primo1 :: Integer -> Bool
primo1 n = divisores1 n == [1, n]

-- 2ª solución
-- ===========

divisoresPrimos2 :: Integer -> [Integer]
divisoresPrimos2 x = [n | n <- divisores2 x, primo2 n]

divisores2 :: Integer -> [Integer]
divisores2 n = xs ++ [n `div` y | y <- ys]
  where xs = primerosDivisores2 n
        (z:zs) = reverse xs
        ys | z^2 == n  = zs
           | otherwise = z:zs

-- (primerosDivisores n) es la lista de los divisores del número n cuyo
-- cuadrado es menor o gual que n. Por ejemplo,
--    primerosDivisores 25  ==  [1,5]
--    primerosDivisores 30  ==  [1,2,3,5]
primerosDivisores2 :: Integer -> [Integer]
primerosDivisores2 n =
   [x | x <- [1..round (sqrt (fromIntegral n))],
        n `mod` x == 0]

primo2 :: Integer -> Bool
primo2 1 = False
primo2 n = primerosDivisores2 n == [1]

-- 3ª solución
-- ===========

divisoresPrimos3 :: Integer -> [Integer]
divisoresPrimos3 x = [n | n <- divisores3 x, primo3 n]

divisores3 :: Integer -> [Integer]
divisores3 n = xs ++ [n `div` y | y <- ys]
  where xs = primerosDivisores3 n
        (z:zs) = reverse xs
        ys | z^2 == n  = zs
           | otherwise = z:zs

primerosDivisores3 :: Integer -> [Integer]
primerosDivisores3 n =
   filter ((== 0) . mod n) [1..round (sqrt (fromIntegral n))]

primo3 :: Integer -> Bool
primo3 1 = False
primo3 n = primerosDivisores3 n == [1]

-- 4ª solución
-- ===========

divisoresPrimos4 :: Integer -> [Integer]
divisoresPrimos4 n
  | even n = 2 : divisoresPrimos4 (reducido n 2)
  | otherwise = aux n [3,5..n]
  where aux 1 _  = []
        aux _ [] = []
        aux m (x:xs) | m `mod` x == 0 = x : aux (reducido m x) xs
                     | otherwise      = aux m xs

-- (reducido m x) es el resultado de dividir repetidamente m por x,
-- mientras sea divisible. Por ejemplo,
--    reducido 36 2  ==  9
reducido :: Integer -> Integer -> Integer
reducido m x | m `mod` x == 0 = reducido (m `div` x) x
             | otherwise      = m

-- 5ª solución
-- ===========

divisoresPrimos5 :: Integer -> [Integer]
divisoresPrimos5 = nub . primeFactors

-- 6ª solución
-- ===========

divisoresPrimos6 :: Integer -> [Integer]
divisoresPrimos6 = filter isPrime . toList . divisors

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisoresPrimos :: Integer -> Property
prop_divisoresPrimos n =
  n > 1 ==>
  all (== divisoresPrimos1 n)
      [divisoresPrimos2 n,
       divisoresPrimos3 n,
       divisoresPrimos4 n,
       divisoresPrimos5 n,
       divisoresPrimos6 n]

-- La comprobación es
--    λ> quickCheck prop_divisoresPrimos
--    +++ OK, passed 100 tests; 108 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> divisoresPrimos1 (product [1..11])
--    [2,3,5,7,11]
--    (18.34 secs, 7,984,382,104 bytes)
--    λ> divisoresPrimos2 (product [1..11])
--    [2,3,5,7,11]
--    (0.02 secs, 2,610,976 bytes)
--    λ> divisoresPrimos3 (product [1..11])
--    [2,3,5,7,11]
--    (0.02 secs, 2,078,288 bytes)
--    λ> divisoresPrimos4 (product [1..11])
--    [2,3,5,7,11]
--    (0.02 secs, 565,992 bytes)
--    λ> divisoresPrimos5 (product [1..11])
--    [2,3,5,7,11]
--    (0.01 secs, 568,000 bytes)
--    λ> divisoresPrimos6 (product [1..11])
--    [2,3,5,7,11]
--    (0.00 secs, 2,343,392 bytes)
--
--    λ> divisoresPrimos2 (product [1..16])
--    [2,3,5,7,11,13]
--    (2.32 secs, 923,142,480 bytes)
--    λ> divisoresPrimos3 (product [1..16])
--    [2,3,5,7,11,13]
--    (0.80 secs, 556,961,088 bytes)
--    λ> divisoresPrimos4 (product [1..16])
--    [2,3,5,7,11,13]
--    (0.01 secs, 572,368 bytes)
--    λ> divisoresPrimos5 (product [1..16])
--    [2,3,5,7,11,13]
--    (0.01 secs, 31,665,896 bytes)
--    λ> divisoresPrimos6 (product [1..16])
--    [2,3,5,7,11,13]
--    (0.01 secs, 18,580,584 bytes)
--
--    λ> length (divisoresPrimos4 (product [1..30]))
--    10
--    (0.01 secs, 579,168 bytes)
--    λ> length (divisoresPrimos5 (product [1..30]))
--    10
--    (0.01 secs, 594,976 bytes)
--    λ> length (divisoresPrimos6 (product [1..30]))
--    10
--    (3.38 secs, 8,068,783,408 bytes)
--
--    λ> length (divisoresPrimos4 (product [1..20000]))
--    2262
--    (1.20 secs, 1,940,069,976 bytes)
--    λ> length (divisoresPrimos5 (product [1..20000]))
--    2262
--    (1.12 secs, 1,955,921,736 bytes)
