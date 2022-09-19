-- Divisores_de_un_numero.hs
-- Divisores de un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisores :: Integer -> [Integer]
-- tal que (divisores x) es el conjunto de divisores de los x. Por
-- ejemplo,
--   divisores 30  ==  [1,2,3,5,6,10,15,30]
--   length (divisores (product [1..10]))  ==  270
--   length (divisores (product [1..25]))  ==  340032
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Divisores_de_un_numero where

import Data.List (group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)
import Data.Set (toList)
import Math.NumberTheory.ArithmeticFunctions (divisors)

import Test.QuickCheck

-- 1ª solución
-- ===========

divisores1 :: Integer -> [Integer]
divisores1 n = [x | x <- [1..n], n `rem` x == 0]

-- 2ª solución
-- ===========

divisores2 :: Integer -> [Integer]
divisores2 n = [x | x <- [1..n], x `esDivisorDe` n]

-- (esDivisorDe x n) se verifica si x es un divisor de n. Por ejemplo,
--    esDivisorDe 2 6  ==  True
--    esDivisorDe 4 6  ==  False
esDivisorDe :: Integer -> Integer -> Bool
esDivisorDe x n = n `rem` x == 0

-- 3ª solución
-- ===========

divisores3 :: Integer -> [Integer]
divisores3 n = filter (`esDivisorDe` n) [1..n]

-- 4ª solución
-- ===========

divisores4 :: Integer -> [Integer]
divisores4 = filter <$> flip esDivisorDe <*> enumFromTo 1

-- 5ª solución
-- ===========

divisores5 :: Integer -> [Integer]
divisores5 n = xs ++ [n `div` y | y <- ys]
  where xs = primerosDivisores1 n
        (z:zs) = reverse xs
        ys | z^2 == n  = zs
           | otherwise = z:zs

-- (primerosDivisores n) es la lista de los divisores del número n cuyo
-- cuadrado es menor o gual que n. Por ejemplo,
--    primerosDivisores 25  ==  [1,5]
--    primerosDivisores 30  ==  [1,2,3,5]
primerosDivisores1 :: Integer -> [Integer]
primerosDivisores1 n =
   [x | x <- [1..round (sqrt (fromIntegral n))],
        x `esDivisorDe` n]

-- 6ª solución
-- ===========

divisores6 :: Integer -> [Integer]
divisores6 n = aux [1..n]
  where aux [] = []
        aux (x:xs) | x `esDivisorDe` n = x : aux xs
                   | otherwise         = aux xs

-- 7ª solución
-- ===========

divisores7 :: Integer -> [Integer]
divisores7 n = xs ++ [n `div` y | y <- ys]
  where xs = primerosDivisores2 n
        (z:zs) = reverse xs
        ys | z^2 == n  = zs
           | otherwise = z:zs

primerosDivisores2 :: Integer -> [Integer]
primerosDivisores2 n = aux [1..round (sqrt (fromIntegral n))]
  where aux [] = []
        aux (x:xs) | x `esDivisorDe` n = x : aux xs
                   | otherwise         = aux xs

-- 8ª solución
-- ===========

divisores8 :: Integer -> [Integer]
divisores8 =
  nub . sort . map product . subsequences . primeFactors

-- 9ª solución
-- ===========

divisores9 :: Integer -> [Integer]
divisores9 = sort
           . map (product . concat)
           . productoCartesiano
           . map inits
           . group
           . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]

-- 10ª solución
-- ============

divisores10 :: Integer -> [Integer]
divisores10 = sort
            . map (product . concat)
            . mapM inits
            . group
            . primeFactors

-- 11ª solución
-- ============

divisores11 :: Integer -> [Integer]
divisores11 = toList . divisors

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisores :: Positive Integer -> Bool
prop_divisores (Positive n) =
  all (== divisores1 n)
      [ divisores2 n
      , divisores3 n
      , divisores4 n
      , divisores5 n
      , divisores6 n
      , divisores7 n
      , divisores8 n
      , divisores9 n
      , divisores10 n
      , divisores11 n
      ]

-- La comprobación es
--    λ> quickCheck prop_divisores
--    +++ OK, passed 100 tests.

-- Comparación de la eficiencia
-- ============================

-- La comparación es
--    λ> length (divisores1 (product [1..11]))
--    540
--    (18.55 secs, 7,983,950,592 bytes)
--    λ> length (divisores2 (product [1..11]))
--    540
--    (18.81 secs, 7,983,950,592 bytes)
--    λ> length (divisores3 (product [1..11]))
--    540
--    (12.79 secs, 6,067,935,544 bytes)
--    λ> length (divisores4 (product [1..11]))
--    540
--    (12.51 secs, 6,067,935,592 bytes)
--    λ> length (divisores5 (product [1..11]))
--    540
--    (0.03 secs, 1,890,296 bytes)
--    λ> length (divisores6 (product [1..11]))
--    540
--    (21.46 secs, 9,899,961,392 bytes)
--    λ> length (divisores7 (product [1..11]))
--    540
--    (0.02 secs, 2,195,800 bytes)
--    λ> length (divisores8 (product [1..11]))
--    540
--    (0.09 secs, 107,787,272 bytes)
--    λ> length (divisores9 (product [1..11]))
--    540
--    (0.02 secs, 2,150,472 bytes)
--    λ> length (divisores10 (product [1..11]))
--    540
--    (0.01 secs, 1,652,120 bytes)
--    λ> length (divisores11 (product [1..11]))
--    540
--    (0.01 secs, 796,056 bytes)
--
--    λ> length (divisores5 (product [1..17]))
--    10752
--    (10.16 secs, 3,773,953,128 bytes)
--    λ> length (divisores7 (product [1..17]))
--    10752
--    (9.83 secs, 4,679,260,712 bytes)
--    λ> length (divisores9 (product [1..17]))
--    10752
--    (0.06 secs, 46,953,344 bytes)
--    λ> length (divisores10 (product [1..17]))
--    10752
--    (0.02 secs, 33,633,712 bytes)
--    λ> length (divisores11 (product [1..17]))
--    10752
--    (0.03 secs, 6,129,584 bytes)
--
--    λ> length (divisores10 (product [1..27]))
--    677376
--    (2.14 secs, 3,291,277,736 bytes)
--    λ> length (divisores11 (product [1..27]))
--    677376
--    (0.56 secs, 396,042,280 bytes)
