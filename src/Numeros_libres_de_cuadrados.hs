-- Numeros_libres_de_cuadrados.hs
-- Números libres de cuadrados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número es libre de cuadrados si no es divisible por el cuadrado de
-- ningún entero mayor que 1. Por ejemplo, 70 es libre de cuadrado
-- porque sólo es divisible por 1, 2, 5, 7 y 70; en cambio, 40 no es
-- libre de cuadrados porque es divisible por 2^2.
--
-- Definir la función
--    libreDeCuadrados :: Integer -> Bool
-- tal que (libreDeCuadrados x) se verifica si x es libre de cuadrados.
-- Por ejemplo,
--    libreDeCuadrados 70  ==  True
--    libreDeCuadrados 40  ==  False
--    libreDeCuadrados (product (take 30000 primes))  ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_libres_de_cuadrados where

import Data.List (nub)
import Data.Numbers.Primes (primeFactors, primes)
import Test.QuickCheck

-- 1ª solución
-- ===========

libreDeCuadrados1 :: Integer -> Bool
libreDeCuadrados1 n =
  null [x | x <- [2..n], rem n (x^2) == 0]

-- 2ª solución
-- ===========

libreDeCuadrados2 :: Integer -> Bool
libreDeCuadrados2 x =
  x == product (divisoresPrimos2 x)

-- (divisoresPrimos x) es la lista de los divisores primos de x. Por
-- ejemplo,
--    divisoresPrimos 40 == [2,5]
--    divisoresPrimos 70 == [2,5,7]
divisoresPrimos2 :: Integer -> [Integer]
divisoresPrimos2 x = [n | n <- divisores2 x, primo2 n]

-- (divisores n) es la lista de los divisores del número n. Por ejemplo,
--    divisores 25  ==  [1,5,25]
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores2 :: Integer -> [Integer]
divisores2 n = [x | x <- [1..n], n `mod` x == 0]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True
primo2 :: Integer -> Bool
primo2 n = divisores2 n == [1, n]

-- 3ª solución
-- ===========

libreDeCuadrados3 :: Integer -> Bool
libreDeCuadrados3 n
  | even n = n `mod` 4 /= 0 && libreDeCuadrados3 (n `div` 2)
  | otherwise = aux n [3,5..n]
  where aux 1 _  = True
        aux _ [] = True
        aux m (x:xs)
          | m `mod` x == 0 = m `mod` (x^2) /= 0 && aux (m `div` x) xs
          | otherwise      = aux m xs

-- 4ª solución
-- ===========

libreDeCuadrados4 :: Integer -> Bool
libreDeCuadrados4 x =
  x == product (divisoresPrimos4 x)

divisoresPrimos4 :: Integer -> [Integer]
divisoresPrimos4 = nub . primeFactors

-- 5ª solución
-- ===========

libreDeCuadrados5 :: Integer -> Bool
libreDeCuadrados5 =
  sinRepetidos . primeFactors

-- (sinRepetidos xs) se verifica si xs no tiene elementos repetidos. Por
-- ejemplo,
--    sinRepetidos [3,2,5]  ==  True
--    sinRepetidos [3,2,5,2]  ==  False
sinRepetidos :: [Integer] -> Bool
sinRepetidos xs =
  nub xs == xs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_libreDeCuadrados :: Integer -> Property
prop_libreDeCuadrados x =
  x > 1 ==>
  all (== libreDeCuadrados1 x)
      [libreDeCuadrados2 x,
       libreDeCuadrados3 x,
       libreDeCuadrados4 x,
       libreDeCuadrados5 x]

-- La comprobación es
--    λ> quickCheck prop_libreDeCuadrados
--    +++ OK, passed 100 tests; 165 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> libreDeCuadrados1 9699690
--    True
--    (8.54 secs, 6,441,144,248 bytes)
--    λ> libreDeCuadrados2 9699690
--    True
--    (4.78 secs, 1,940,781,632 bytes)
--    λ> libreDeCuadrados3 9699690
--    True
--    (0.01 secs, 561,400 bytes)
--    λ> libreDeCuadrados4 9699690
--    True
--    (0.01 secs, 568,160 bytes)
--    λ> libreDeCuadrados5 9699690
--    True
--    (0.01 secs, 567,536 bytes)
--
--    λ> libreDeCuadrados3 (product (take 30000 primes))
--    True
--    (2.30 secs, 2,369,316,208 bytes)
--    λ> libreDeCuadrados4 (product (take 30000 primes))
--    True
--    (6.68 secs, 4,565,617,408 bytes)
--    λ> libreDeCuadrados5 (product (take 30000 primes))
--    True
--    (5.54 secs, 3,411,701,752 bytes)
