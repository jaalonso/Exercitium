-- Suma_de_cuadrados_menos_cuadrado_de_la_suma.hs
-- Suma de cuadrados menos cuadrado de la suma
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    euler6 :: Integer -> Integer
-- tal que (euler6 n) es la diferencia entre el cuadrado de la suma
-- de los n primeros números y la suma de los cuadrados de los n
-- primeros números. Por ejemplo,
--    euler6 10       ==  2640
--    euler6 (10^10)  ==  2500000000166666666641666666665000000000
--
-- Nota: Este ejercicio está basado en el problema 6 del proyecto Euler
-- https://www.projecteuler.net/problem=6
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Suma_de_cuadrados_menos_cuadrado_de_la_suma where

import Suma_de_los_cuadrados_de_los_primeros_numeros_naturales
  (sumaDeCuadrados1, sumaDeCuadrados2, sumaDeCuadrados3, sumaDeCuadrados4, sumaDeCuadrados5)
import Data.List (foldl')
import Test.QuickCheck

-- 1ª solución
-- ===========

euler6a :: Integer -> Integer
euler6a n = suma1 n ^ 2 - sumaDeCuadrados1 n

-- (suma n) es la suma de los n primeros números. Por ejemplo,
--    suma 3  ==  6
suma1 :: Integer -> Integer
suma1 n = sum [1..n]

-- 2ª solución
-- ===========

euler6b :: Integer -> Integer
euler6b n = suma2 n ^ 2 - sumaDeCuadrados2 n

suma2 :: Integer -> Integer
suma2 n = (1+n)*n `div` 2

-- 3ª solución
-- ===========

euler6c :: Integer -> Integer
euler6c n = suma3 n ^ 2 - sumaDeCuadrados3 n

suma3 :: Integer -> Integer
suma3 1 = 1
suma3 n = n + suma3 (n-1)

-- 4ª solución
-- ===========

euler6d :: Integer -> Integer
euler6d n = suma4 n ^ 2 - sumaDeCuadrados4 n

suma4 :: Integer -> Integer
suma4 n = foldl (+) 0 [0..n]

-- 5ª solución
-- ===========

euler6e :: Integer -> Integer
euler6e n = suma5 n ^ 2 - sumaDeCuadrados5 n

suma5 :: Integer -> Integer
suma5 n = foldl' (+) 0 [0..n]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_euler6 :: Positive Integer -> Bool
prop_euler6 (Positive n) =
  all (== euler6a n)
      [euler6b n,
       euler6c n,
       euler6d n,
       euler6e n]

-- La comprobación es
--    λ> quickCheck prop_euler6
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> euler6a (3*10^6)
--    20250004499997749999500000
--    (3.32 secs, 2,577,174,640 bytes)
--    λ> euler6b (3*10^6)
--    20250004499997749999500000
--    (0.01 secs, 569,288 bytes)
--    λ> euler6c (3*10^6)
--    20250004499997749999500000
--    (5.60 secs, 2,849,479,288 bytes)
--    λ> euler6d (3*10^6)
--    20250004499997749999500000
--    (2.52 secs, 2,457,175,248 bytes)
--    λ> euler6e (3*10^6)
--    20250004499997749999500000
--    (1.08 secs, 2,016,569,472 bytes)
--
--    λ> euler6a (10^7)
--    2500000166666641666665000000
--    (11.14 secs, 8,917,796,648 bytes)
--    λ> euler6b (10^7)
--    2500000166666641666665000000
--    (0.01 secs, 570,752 bytes)
--    λ> euler6c (10^7)
--    *** Exception: stack overflow
--    λ> euler6d (10^7)
--    2500000166666641666665000000
--    (9.47 secs, 8,517,796,760 bytes)
--    λ> euler6e (10^7)
--    2500000166666641666665000000
--    (3.78 secs, 7,049,100,104 bytes)
