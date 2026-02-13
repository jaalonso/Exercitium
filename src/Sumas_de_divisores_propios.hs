-- Sumas_de_divisores_propios.hs
-- Sumas de divisores propios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-Febrero-2015 (actualizado 13-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDivisoresHasta :: Int -> [(Int,Int)]
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
import Math.NumberTheory.ArithmeticFunctions (sigma)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Fuerza bruta
-- =========================

sumaDivisoresHasta1 :: Int -> [(Int,Int)]
sumaDivisoresHasta1 n =
  [(x, sum (divisores x)) | x <- [1..n]]

divisores :: Int -> [Int]
divisores n =
  [x | x <- [1..n `div` 2], n `mod` x == 0]

-- 2ª solución: Basada en factorización
-- ====================================

sumaDivisoresHasta2 :: Int -> [(Int,Int)]
sumaDivisoresHasta2 n =
  [(x, sumaDivisores x) | x <- [1..n]]

sumaDivisores :: Int -> Int
sumaDivisores x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x] - x

-- (factorizacion x) es la lista de las bases y exponentes de la
-- descomposición prima de x. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Int -> [(Int,Int)]
factorizacion =
  map primeroYlongitud . group . primeFactors

-- (primeroYlongitud xs) es el par formado por el primer elemento de xs
-- y la longitud de xs. Por ejemplo,
--    primeroYlongitud [3,2,5,7] == (3,4)
primeroYlongitud :: [a] -> (a,Int)
primeroYlongitud (x:xs) =
  (x, 1 + genericLength xs)

-- 3ª solución: Usando sigma
-- =========================

sumaDivisoresHasta3 :: Int -> [(Int,Int)]
sumaDivisoresHasta3 n =
  [(x, sumaDivisores3 x) | x <- [1..n]]

sumaDivisores3 :: Int -> Int
sumaDivisores3 x = sigma 1 x - x

-- 4ª solución: Criba con accumArray
-- =================================

sumaDivisoresHasta4 :: Int -> [(Int,Int)]
sumaDivisoresHasta4 n =
  assocs (accumArray (+) 0 (1,n) (divisoresHasta n))

-- (divisoresHasta n) es la lista de los pares (a,b) tales que a es
-- un número entre 2 y n y b es un divisor propio e x. Por ejemplo,
--    λ> divisoresHasta 6
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(4,2),(6,2),(6,3)]
--    λ> divisoresHasta 8
--    [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(4,2),(6,2),(8,2),(6,3),(8,4)]
divisoresHasta :: Int -> [(Int,Int)]
divisoresHasta n =
  [(a,b) | b <- [1..n `div` 2], a <- [b*2, b*3..n]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [(Int,Int)]) -> Spec
specG sumaDivisoresHasta = do
  it "e1" $
    sumaDivisoresHasta 12 `shouldBe`
    [(1,0),(2,1),(3,1),(4,3),(5,1),(6,6),(7,1),(8,7),(9,4),(10,8),(11,1),(12,16)]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDivisoresHasta1
  describe "def. 2" $ specG sumaDivisoresHasta2
  describe "def. 3" $ specG sumaDivisoresHasta3
  describe "def. 4" $ specG sumaDivisoresHasta4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaDivisoresHasta :: Positive Int -> Bool
prop_sumaDivisoresHasta (Positive n) =
  all (== sumaDivisoresHasta1 n)
      [ sumaDivisoresHasta2 n
      , sumaDivisoresHasta3 n
      , sumaDivisoresHasta4 n
      ]

-- La comprobación es
--    λ> quickCheck prop_sumaDivisoresHasta
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximum (map snd (sumaDivisoresHasta1 5000))
--    11700
--    (2.41 secs, 1,256,416,456 bytes)
--    λ> maximum (map snd (sumaDivisoresHasta2 5000))
--    11700
--    (0.09 secs, 101,568,864 bytes)
--    λ> maximum (map snd (sumaDivisoresHasta3 5000))
--    11700
--    (0.02 secs, 16,444,288 bytes)
--    λ> maximum (map snd (sumaDivisoresHasta4 5000))
--    11700
--    (0.04 secs, 10,616,024 bytes)
--
--    λ> maximum (map snd (sumaDivisoresHasta2 100000))
--    304920
--    (2.14 secs, 4,485,878,400 bytes)
--    λ> maximum (map snd (sumaDivisoresHasta3 100000))
--    304920
--    (0.19 secs, 343,279,536 bytes)
--    λ> maximum (map snd (sumaDivisoresHasta4 100000))
--    304920
--    (0.37 secs, 262,899,320 bytes)
--
-- La comparación de (maximum (map snd (f 5000))) es:
--    | Solución | Tiempo | Memoria   |
--    |----------|--------|-----------|
--    | Def. 1   | 2.41 s | 1256.4 MB |
--    | Def. 2   | 0.09 s |  101.6 MB |
--    | Def. 3   | 0.02 s |   16.4 MB |
--    | Def. 4   | 0.04 s |   10.6 MB |

-- La comparación de (maximum (map snd (f 100000))) es:
--    | Solución | Tiempo | Memoria |
--    |----------|--------|---------|
--    | Def. 2   | 2.14 s | 4.48 GB |
--    | Def. 3   | 0.19 s | 0.34 GB |
--    | Def. 4   | 0.37 s | 0.26 GB |
