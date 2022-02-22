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

import Test.QuickCheck (Property, (==>), quickCheck)

-- 1ª solución
-- ===========

cuadradoCercano :: Integer -> Integer
cuadradoCercano n
  | n - a^2 < (a+1)^2 - n = a^2
  | otherwise             = (a+1)^2
  where a = raizEntera n

-- (raizEntera x) es el mayor entero cuyo cuadrado no es mayor que
-- x. Por ejemplo,
--    raizEntera 8   ==  2
--    raizEntera 9   ==  3
--    raizEntera 10  ==  3
raizEntera :: Integer -> Integer
raizEntera x = aux (1,x)
    where aux (a,b) | d == x    = c
                    | c == a    = c
                    | d < x     = aux (c,b)
                    | otherwise = aux (a,c)
              where c = (a+b) `div` 2
                    d = c^2

-- 2ª solución
-- ===========

cuadradoCercano2 :: Integer -> Integer
cuadradoCercano2 n
  | n - a^2 < (a+1)^2 - n = a^2
  | otherwise             = (a+1)^2
  where a = raizEntera2 n

-- (raizEntera2 x) es el mayor entero cuyo cuadrado no es mayor que
-- x. Por ejemplo,
--    raizEntera2 8   ==  2
--    raizEntera2 9   ==  3
--    raizEntera2 10  ==  3
raizEntera2 :: Integer -> Integer
raizEntera2 0 = 0
raizEntera2 1 = 1
raizEntera2 n = head (dropWhile supera (iterate mejora (div n 2)))
  where mejora x = div (x + div n x) 2
        supera x = x^2 > n

-- 3ª solución
-- ===========

cuadradoCercano3 :: Integer -> Integer
cuadradoCercano3 = (^ 2) . round . sqrt . fromIntegral

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_cuadradoCercano :: Integer -> Property
prop_cuadradoCercano x =
  x >= 0 ==> cuadradoCercano3 x == cuadradoCercano x

verifica_cuadradoCercano :: IO ()
verifica_cuadradoCercano = quickCheck prop_cuadradoCercano

-- La comprobación es
--    λ> verifica_cuadradoCercano
--    +++ OK, passed 100 tests.

-- Aunque ha pasado los 100 tests, las definiciones no son
-- equivalentes. Por ejemplo,
--    λ> cuadradoCercano3 (10^46) == cuadradoCercano (10^46)
--    False
--    λ> cuadradoCercano3 (10^46)
--    9999999999999998322278400000000070368744177664
--    λ> cuadradoCercano (10^46)
--    10000000000000000000000000000000000000000000000
