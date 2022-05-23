-- Sumas_de_divisores_propios.hs
-- Sumas de divisores propios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDivisoresHasta :: Integer -> [(Integer,Integer)]
-- tal que (sumaDivisoresHasta n) es la lista de los pares (a,b) tales
-- que a es un número entre 1 y n y b es la suma de los divisores
-- propios de a. Por ejemplo,
--    λ> sumaDivisoresHasta 12
--    [(1,0),(2,1),(3,1),(4,3),(5,1),(6,6),(7,1),(8,7),(9,4),(10,8),(11,1),(12,16)]
--    λ> last (sumaDivisoresHasta2 (10^7))
--    (10000000,14902280)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sumas_de_divisores_propios where

import Data.Array (accumArray, assocs)
import Data.List (genericLength, group)
import Data.Numbers.Primes (primeFactors)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaDivisoresHasta1 :: Integer -> [(Integer,Integer)]
sumaDivisoresHasta1 n = [(x, sum (divisores x)) | x <- [1..n]]

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n `div` 2], n `mod` x == 0]

-- 2ª solución
-- ===========

sumaDivisoresHasta2 :: Integer -> [(Integer,Integer)]
sumaDivisoresHasta2 n = [(x, sumaDivisores x) | x <- [1..n]]

sumaDivisores :: Integer -> Integer
sumaDivisores x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x] - x

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

-- 3ª solución
-- ===========

sumaDivisoresHasta3 :: Integer -> [(Integer,Integer)]
sumaDivisoresHasta3 n = assocs (accumArray (+) 0 (1,n) (divisoresHasta n))

-- (divisoresHasta n) es la lista de los pares (a,b) tales que a es
-- un número entre 2 y n y b es un divisor propio e x. Por ejemplo,
--    λ> divisoresHasta 6
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(4,2),(6,2),(6,3)]
--    λ> divisoresHasta 8
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(4,2),(6,2),(8,2),(6,3),(8,4)]
divisoresHasta :: Integer -> [(Integer,Integer)]
divisoresHasta n = [(a,b) | b <- [1..n `div` 2], a <- [b*2, b*3..n]]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaDivisoresHasta :: Positive Integer -> Bool
prop_sumaDivisoresHasta (Positive n) =
  all (== sumaDivisoresHasta1 n)
      [ sumaDivisoresHasta2 n
      , sumaDivisoresHasta3 n
      ]

-- La comprobación es
--    λ> quickCheck prop_sumaDivisoresHasta
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (sumaDivisoresHasta1 (10^6))
--    (1000000,1480437)
--    (0.47 secs, 308,542,392 bytes)
--    λ> last (sumaDivisoresHasta2 (10^6))
--    (1000000,2480437)
--    (0.26 secs, 208,548,944 bytes)
--    λ> last (sumaDivisoresHasta3 (10^6))
--    (1000000,1480437)
--    (6.65 secs, 3,249,831,856 bytes)
--
--    λ> last (sumaDivisoresHasta1 (5*10^6))
--    (5000000,7402312)
--    (2.27 secs, 1,540,543,352 bytes)
--    λ> last (sumaDivisoresHasta2 (5*10^6))
--    (5000000,12402312)
--    (1.19 secs, 1,040,549,800 bytes)
