-- Cuadrado_mas_cercano.hs
-- Cuadrado más cercano.
-- José A. Alonso Jiménez
-- Sevilla, 26-enero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    cuadradoCercano :: Integer -> Integer
-- tal que (cuadradoCercano n) es el número cuadrado más cercano a n,
-- donde n es un entero positivo. Por ejemplo,
--    cuadradoCercano 2       == 1
--    cuadradoCercano 6       == 4
--    cuadradoCercano 8       == 9
--    cuadradoCercano (10^46) == 10000000000000000000000000000000000000000000000
-- ---------------------------------------------------------------------

module Cuadrado_mas_cercano where

import Test.QuickCheck

-- 1ª solución
-- ===========

cuadradoCercano1 :: Integer -> Integer
cuadradoCercano1 n
  | n - b < c - n = b
  | otherwise     = c
  where a = raizEntera1 n
        b = a^2
        c = (a+1)^2

-- (raizEntera x) es el mayor entero cuyo cuadrado no es mayor que
-- x. Por ejemplo,
--    raizEntera 8   ==  2
--    raizEntera 9   ==  3
--    raizEntera 10  ==  3
raizEntera1 :: Integer -> Integer
raizEntera1 x =
  last (takeWhile (\n -> n^2 <= x) [1..])

-- 2ª solución
-- ===========

cuadradoCercano2 :: Integer -> Integer
cuadradoCercano2 n
  | n - b < c - n = b
  | otherwise     = c
  where a = raizEntera2 n
        b = a^2
        c = (a+1)^2

raizEntera2 :: Integer -> Integer
raizEntera2 x = aux (1,x)
    where aux (a,b) | d == x    = c
                    | c == a    = c
                    | x <= d    = aux (a,c)
                    | otherwise = aux (c,b)
            where c = (a+b) `div` 2
                  d = c^2

-- 3ª solución
-- ===========

cuadradoCercano3 :: Integer -> Integer
cuadradoCercano3 n
  | n - b < c - n = b
  | otherwise     = c
  where a = raizEntera3 n
        b = a^2
        c = (a+1)^2

raizEntera3 :: Integer -> Integer
raizEntera3 0 = 0
raizEntera3 1 = 1
raizEntera3 n = until aceptable mejora n
  where mejora x    = (x + n `div` x) `div` 2
        aceptable x = x^2 <= n

-- 4ª solución
-- ===========

cuadradoCercano4 :: Integer -> Integer
cuadradoCercano4 = (^ 2) . round . sqrt . fromIntegral

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_cuadradoCercano :: Positive Integer -> Bool
prop_cuadradoCercano (Positive x) =
  all (== cuadradoCercano1 x)
      [cuadradoCercano2 x,
       cuadradoCercano3 x,
       cuadradoCercano4 x]

-- La comprobación es
--    λ> quickCheck prop_cuadradoCercano
--    +++ OK, passed 100 tests.

-- Aunque ha pasado los 100 tests, las definiciones no son
-- equivalentes. Por ejemplo,
--    λ> cuadradoCercano3 (10^46) == cuadradoCercano (10^46)
--    False
--    λ> cuadradoCercano3 (10^46)
--    9999999999999998322278400000000070368744177664
--    λ> cuadradoCercano (10^46)
--    10000000000000000000000000000000000000000000000

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> cuadradoCercano1 (10^14)
--    100000000000000
--    (4.59 secs, 5,920,475,784 bytes)
--    λ> cuadradoCercano2 (10^14)
--    100000000000000
--    (0.01 secs, 512,472 bytes)
--    λ> cuadradoCercano3 (10^14)
--    100000000000000
--    (0.01 secs, 494,248 bytes)
--    λ> cuadradoCercano4 (10^14)
--    100000000000000
--    (0.01 secs, 475,152 bytes)
--
--    λ> length (show (cuadradoCercano2 (10^20000)))
--    20001
--    (3.94 secs, 1,446,675,504 bytes)
--    λ> length (show (cuadradoCercano3 (10^20000)))
--    20001
--    (4.50 secs, 926,647,904 bytes)
