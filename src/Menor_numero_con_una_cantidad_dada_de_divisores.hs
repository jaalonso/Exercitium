-- Menor_numero_con_una_cantidad_dada_de_divisores.hs
-- Menor número con una cantidad dada de divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El menor número con 2 divisores es el 2, ya que tiene 2 divisores (el
-- 1 y el 2) y el anterior al 2 (el 1) sólo tiene 1 divisor (el 1).
--
-- El menor número con 4 divisores es el 6, ya que tiene 4 divisores (el
-- 1, 2, 3 y 6) y sus anteriores (el 1, 2, 3, 4 y 5) tienen menos de 4
-- divisores (tienen 1, 1, 1, 3 y 1, respectivamente).
--
-- El menor número con 8 divisores es el 24, ya que tiene 8 divisores
-- (el 1, 2, 3, 4, 6, 8, 12 y 24) y sus anteriores (del 1 al 23) tienen
-- menos de 8 divisores.
--
-- El menor número con 16 divisores es el 120, ya que tiene 16 divisores
-- (el 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60 y 120) y sus
-- anteriores (del 1 al 119) tienen menos de 16 divisores.
--
-- Definir la función
--    menor :: Integer -> Integer
-- tal que (menor n) es el menor número con 2^n divisores. Por ejemplo,
--    menor 1  ==  2
--    menor 2  ==  6
--    menor 3  ==  24
--    menor 4  ==  120
--    length (show (menor (4*10^4)))  ==  207945
--
-- Comprobar con QuickCheck que, para todo k >=0, (menor (2^k)) es un
-- divisor de (menor (2^(k+1))).
--
-- Nota: Este ejercicio está basado en el problema N1 de la Olimpíada
-- Internacional de Matemáticas (IMO) del 2011 https://bit.ly/2LhS0w8
-- --------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Menor_numero_con_una_cantidad_dada_de_divisores where

import Data.List           (genericLength, genericTake, group)
import Data.Numbers.Primes (primeFactors, primes)
import Test.QuickCheck     (Positive (Positive), maxSize, stdArgs,
                            quickCheck, quickCheckWith)

-- 1ª solución
-- ===========

menor1 :: Integer -> Integer
menor1 n =
  head [x | x <- [1..], numeroDivisores x == 2^n]

-- (numeroDivisores n) es el número de divisores de n. Por ejemplo, 
--    numeroDivisores 12  ==  6
numeroDivisores :: Integer -> Integer
numeroDivisores =
  genericLength . divisores

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 12  ==  [1,3,2,6,4,12]
--    divisores 25  ==  [1,5,25]
divisores :: Integer -> [Integer]
divisores n =
  [x | x <- [1..n], n `rem` x == 0]

-- 2ª solución
-- ===========

menor2 :: Integer -> Integer
menor2 n =
  head [x | x <- [1..], numeroDivisores2 x == 2^n]

numeroDivisores2 :: Integer -> Integer
numeroDivisores2 =
  product . map ((+1) . genericLength) . group . primeFactors

-- 3ª solución
-- ===========

menor3 :: Integer -> Integer
menor3 n = product (genericTake n potencias)

-- potencias es la sucesión de las potencias de la forma p^(2^k),
-- donde p es un número primo y k es un número natural, ordenadas de
-- menor a mayor. Por ejemplo,
--    take 14 potencias    ==  [2,3,4,5,7,9,11,13,16,17,19,23,25,29]
potencias :: [Integer]
potencias = 2 : mezcla (tail primes) (map (^2) potencias)

-- (mezcla xs ys) es la lista obtenida mezclando las dos listas xs e ys,
-- que se suponen ordenadas y disjuntas. Por ejemplo,
--    λ> take 15 (mezcla [2^n | n <- [1..]] [3^n | n <- [1..]])
--    [2,3,4,8,9,16,27,32,64,81,128,243,256,512,729]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla (x:xs) (y:ys) | x < y = x : mezcla xs (y:ys)
                     | x > y = y : mezcla (x:xs) ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_menor :: Positive Integer -> Bool
prop_menor (Positive n) =
  all (== menor1 n)
      [menor2 n,
       menor3 n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=5}) prop_menor
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--   λ> menor 8
--   1081080
--   (47.69 secs, 94,764,856,352 bytes)
--   λ> menor2 8
--   1081080
--   (36.17 secs, 94,764,856,368 bytes)
--   λ> menor3 8
--   1081080
--   (0.00 secs, 116,960 bytes)

-- Definición de menor
-- ===================

-- En lo que sigue, usaremos menor3 como menor.
menor :: Integer -> Integer
menor = menor3

-- Propiedad
-- =========

-- La propiedad es
prop_menor_divide :: Positive Integer -> Bool
prop_menor_divide (Positive n) =
  menor (n+1) `mod` menor n == 0

-- La comprobación es
--    λ> quickCheck prop_menor_divide
--    +++ OK, passed 100 tests.
