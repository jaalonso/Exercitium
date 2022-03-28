-- Conjunto_de_primos_relativos.hs
-- Conjunto de primos relativos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dos números enteros positivos son [primos relativos](http://bit.ly/1xgqDTK)
-- si no tienen ningún factor primo en común; es decit, si 1 es su único
-- divisor común.  Por ejemplo, 6 y 35 son primos entre sí, pero 6 y 27
-- no lo son porque ambos son divisibles por 3.
--
-- Definir la función
--    primosRelativos :: [Int] -> Bool
-- tal que (primosRelativos xs) se verifica si los elementos de xs son
-- primos relativos dos a dos. Por ejemplo,
--    primosRelativos [6,35]         ==  True
--    primosRelativos [6,27]         ==  False
--    primosRelativos [2,3,4]        ==  False
--    primosRelativos [6,35,11]      ==  True
--    primosRelativos [6,35,11,221]  ==  True
--    primosRelativos [6,35,11,231]  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Conjunto_de_primos_relativos where

import Test.QuickCheck
import Data.List (delete, intersect)
import Data.Numbers.Primes (primeFactors, primes)
import qualified Data.Set as S (disjoint, fromList)

-- 1ª solución
-- ===========

primosRelativos1 :: [Int] -> Bool
primosRelativos1 []     = True
primosRelativos1 (x:xs) =
  and [sonPrimosRelativos1 x y | y <- xs] && primosRelativos1 xs

-- (sonPrimosRelativos x y) se verifica si x e y son primos
-- relativos. Por ejemplo,
--    sonPrimosRelativos1 6 35  ==  True
--    sonPrimosRelativos1 6 27  ==  False
sonPrimosRelativos1 :: Int -> Int -> Bool
sonPrimosRelativos1 x y =
  null (divisoresPrimos x `intersect` divisoresPrimos y)

-- (divisoresPrimos x) es la lista de los divisores primos de x. Por
-- ejemplo,
--    divisoresPrimos 600  ==  [2,2,2,3,5,5]
divisoresPrimos :: Int -> [Int]
divisoresPrimos 1 = []
divisoresPrimos x =
  y : divisoresPrimos (x `div` y)
  where y = menorDivisorPrimo x

-- (menorDivisorPrimo x) es el menor divisor primo de x. Por ejemplo,
--    menorDivisorPrimo 15  ==  3
--    menorDivisorPrimo 11  ==  11
menorDivisorPrimo :: Int -> Int
menorDivisorPrimo x =
  head [y | y <- [2..], x `mod` y == 0]

-- 2ª solución
-- ===========

primosRelativos2 :: [Int] -> Bool
primosRelativos2 []     = True
primosRelativos2 (x:xs) =
  all (sonPrimosRelativos1 x) xs && primosRelativos2 xs

-- 3ª solución
-- ===========

primosRelativos3 :: [Int] -> Bool
primosRelativos3 []     = True
primosRelativos3 (x:xs) =
  all (sonPrimosRelativos2 x) xs && primosRelativos3 xs

sonPrimosRelativos2 :: Int -> Int -> Bool
sonPrimosRelativos2 x y =
  null (primeFactors x `intersect` primeFactors y)

-- 4ª solución
-- ===========

primosRelativos4 :: [Int] -> Bool
primosRelativos4 []     = True
primosRelativos4 (x:xs) =
  all (sonPrimosRelativos3 x) xs && primosRelativos4 xs

sonPrimosRelativos3 :: Int -> Int -> Bool
sonPrimosRelativos3 x y =
  S.fromList (primeFactors x) `S.disjoint` S.fromList (primeFactors y)

-- 5ª solución
-- ===========

primosRelativos5 :: [Int] -> Bool
primosRelativos5 []     = True
primosRelativos5 (x:xs) =
  all (sonPrimosRelativos5 x) xs && primosRelativos5 xs

sonPrimosRelativos5 :: Int -> Int -> Bool
sonPrimosRelativos5 x y =
  gcd x y == 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_primosRelativos :: [Positive Int] -> Bool
prop_primosRelativos xs =
  all (== primosRelativos1 ys)
      [primosRelativos2 ys,
       primosRelativos3 ys,
       primosRelativos4 ys,
       primosRelativos5 ys]
  where ys = getPositive <$> xs

-- La comprobación es
--    λ> quickCheck prop_primosRelativos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosRelativos1 (take 120 primes)
--    True
--    (1.92 secs, 869,909,416 bytes)
--    λ> primosRelativos2 (take 120 primes)
--    True
--    (1.99 secs, 869,045,656 bytes)
--    λ> primosRelativos3 (take 120 primes)
--    True
--    (0.09 secs, 221,183,200 bytes)
--
--    λ> primosRelativos3 (take 600 primes)
--    True
--    (2.62 secs, 11,196,690,856 bytes)
--    λ> primosRelativos4 (take 600 primes)
--    True
--    (2.66 secs, 11,190,940,456 bytes)
--    λ> primosRelativos5 (take 600 primes)
--    True
--    (0.14 secs, 123,673,648 bytes)
