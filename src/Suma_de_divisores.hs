-- Suma_de_divisores.hs
-- Suma de divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDivisores :: Integer -> Integer
-- tal que (sumaDivisores x) es la suma de los divisores de x. Por ejemplo,
--    sumaDivisores 12                 ==  28
--    sumaDivisores 25                 ==  31
--    sumaDivisores (product [1..25])  ==  93383273455325195473152000
--    length (show (sumaDivisores (product [1..30000])))  ==  121289
--    maximum (map sumaDivisores [1..2*10^6])             ==  8851392
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Suma_de_divisores where

import Data.List (foldl', genericLength, group, inits)
import Data.Set (toList)
import Data.Numbers.Primes (primeFactors)
import Math.NumberTheory.ArithmeticFunctions (divisors, sigma)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaDivisores1 :: Integer -> Integer
sumaDivisores1 n = sum (divisores1 n)

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 60  ==  [1,5,3,15,2,10,6,30,4,20,12,60]
divisores1 :: Integer -> [Integer]
divisores1 n = [x | x <- [1..n], n `rem` x == 0]

-- 2ª solución
-- ===========

-- Sustituyendo la definición de divisores de la solución anterior por
-- cada una de las del ejercicio [Divisores de un número](https://bit.ly/3S1HYwi)
-- Se obtiene una nueva definición de sumaDivisores. La usada en la
-- definición anterior es la menos eficiente y la que se usa en la
-- siguiente definición es la más eficiente.

sumaDivisores2 :: Integer -> Integer
sumaDivisores2 = sum . divisores2

divisores2 :: Integer -> [Integer]
divisores2 = toList . divisors

-- 3ª solución
-- ===========

-- La solución anterior se puede simplificar

sumaDivisores3 :: Integer -> Integer
sumaDivisores3 = sum . divisors

-- 4ª solución
-- ===========

sumaDivisores4 :: Integer -> Integer
sumaDivisores4 = foldl' (+) 0 . divisores2

-- 5ª solución
-- ===========

sumaDivisores5 :: Integer -> Integer
sumaDivisores5 n = aux [1..n]
  where aux [] = 0
        aux (x:xs) | n `rem` x == 0 = x + aux xs
                   | otherwise      = aux xs

-- 6ª solución
-- ===========

sumaDivisores6 :: Integer -> Integer
sumaDivisores6 = sum
               . map (product . concat)
               . mapM inits
               . group
               . primeFactors

-- 7ª solución
-- ===========

-- Si la descomposición de x en factores primos es
--    x = p(1)^e(1) . p(2)^e(2) . .... . p(n)^e(n)
-- entonces la suma de los divisores de x es
--    p(1)^(e(1)+1) - 1     p(2)^(e(2)+1) - 1       p(n)^(e(2)+1) - 1
--   ------------------- . ------------------- ... -------------------
--        p(1)-1                p(2)-1                  p(n)-1
-- Ver la demostración en http://bit.ly/2zUXZPc

sumaDivisores7 :: Integer -> Integer
sumaDivisores7 x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x]

-- (factorizacion x) es la lista de las bases y exponentes de la
-- descomposición prima de x. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion = map primeroYlongitud . group . primeFactors

-- (primeroYlongitud xs) es el par formado por el primer elemento de xs
-- y la longitud de xs. Por ejemplo,
--    primeroYlongitud [3,2,5,7] == (3,4)
primeroYlongitud :: [a] -> (a,Integer)
primeroYlongitud (x:xs) =
  (x, 1 + genericLength xs)

-- 8ª solución
-- ===========

sumaDivisores8 :: Integer -> Integer
sumaDivisores8 = sigma 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaDivisores :: Positive Integer -> Bool
prop_sumaDivisores (Positive x) =
  all (== sumaDivisores1 x)
      [ sumaDivisores2 x
      , sumaDivisores3 x
      , sumaDivisores4 x
      , sumaDivisores5 x
      , sumaDivisores6 x
      , sumaDivisores7 x
      , sumaDivisores8 x
      ]

-- La comprobación es
--    λ> quickCheck prop_sumaDivisores
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaDivisores1 5336100
--    21386001
--    (2.25 secs, 1,067,805,248 bytes)
--    λ> sumaDivisores2 5336100
--    21386001
--    (0.01 secs, 659,112 bytes)
--    λ> sumaDivisores3 5336100
--    21386001
--    (0.01 secs, 635,688 bytes)
--    λ> sumaDivisores4 5336100
--    21386001
--    (0.01 secs, 648,992 bytes)
--    λ> sumaDivisores5 5336100
--    21386001
--    (2.44 secs, 1,323,924,176 bytes)
--    λ> sumaDivisores6 5336100
--    21386001
--    (0.01 secs, 832,104 bytes)
--    λ> sumaDivisores7 5336100
--    21386001
--    (0.01 secs, 571,040 bytes)
--    λ> sumaDivisores8 5336100
--    21386001
--    (0.00 secs, 558,296 bytes)
--
--    λ> sumaDivisores2 251888923423315469521109880000000
--    1471072204661054993275791673480320
--    (2.30 secs, 1,130,862,080 bytes)
--    λ> sumaDivisores3 251888923423315469521109880000000
--    1471072204661054993275791673480320
--    (1.83 secs, 896,386,232 bytes)
--    λ> sumaDivisores4 251888923423315469521109880000000
--    1471072204661054993275791673480320
--    (1.52 secs, 997,992,328 bytes)
--    λ> sumaDivisores6 251888923423315469521109880000000
--    1471072204661054993275791673480320
--    (2.35 secs, 5,719,848,600 bytes)
--    λ> sumaDivisores7 251888923423315469521109880000000
--    1471072204661054993275791673480320
--    (0.00 secs, 628,136 bytes)
--    λ> sumaDivisores8 251888923423315469521109880000000
--    1471072204661054993275791673480320
--    (0.00 secs, 591,352 bytes)
--
--    λ> length (show (sumaDivisores7 (product [1..30000])))
--    121289
--    (2.76 secs, 4,864,576,304 bytes)
--    λ> length (show (sumaDivisores8 (product [1..30000])))
--    121289
--    (1.65 secs, 3,173,319,312 bytes)
