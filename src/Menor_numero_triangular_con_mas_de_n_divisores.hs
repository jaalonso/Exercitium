-- Menor_numero_triangular_con_mas_de_n_divisores.hs
-- Menor número triangular con más de n divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 06-Febrero-2015 (actualizado 31-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales.
--
--    *     *      *        *         *
--         * *    * *      * *       * *
--               * * *    * * *     * * *
--                       * * * *   * * * *
--                                * * * * *
--    1     3      6        10        15
--
-- Así, el 7º número triangular es
--    1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
-- Los primeros 10 números triangulares son
--    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- Los divisores de los primeros 7 números triangulares son:
--     1: 1
--     3: 1,3
--     6: 1,2,3,6
--    10: 1,2,5,10
--    15: 1,3,5,15
--    21: 1,3,7,21
--    28: 1,2,4,7,14,28
-- Como se puede observar, 28 es el menor número triangular con más de 5
-- divisores.
--
-- Definir la función
--    menorTriangularConAlMenosNDivisores :: Int -> Integer
-- tal que (menorTriangularConAlMenosNDivisores n) es el menor número
-- triangular que tiene al menos n divisores. Por ejemplo,
--    menorTriangularConAlMenosNDivisores 5    == 28
--    menorTriangularConAlMenosNDivisores 50   == 25200
--    menorTriangularConAlMenosNDivisores 500  == 76576500
--    menorTriangularConAlMenosNDivisores 1500 == 7589181600
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Menor_numero_triangular_con_mas_de_n_divisores where

import Data.List (group)
import Data.Numbers.Primes (primes,primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Fuerza bruta (Definición directa)
-- ==============================================

menorTriangularConAlMenosNDivisores1 :: Int -> Integer
menorTriangularConAlMenosNDivisores1 n =
  head [x | x <- triangulares, nDivisores x >= n]

-- triangulares es la sucesión de los números triangulares. Por ejemplo,
--    take 10 triangulares  ==  [1,3,6,10,15,21,28,36,45,55]
triangulares :: [Integer]
triangulares = scanl (+) 1 [2..]

-- (nDivisores x) es el número de divisores de x. Por ejemplo,
--    nDivisores 28  ==  6
nDivisores :: Integer -> Int
nDivisores x =
  1 + length [y | y <- [1..x `div` 2], mod x y == 0]

-- 2ª solución: Factorización prima manual
-- =======================================

menorTriangularConAlMenosNDivisores2 :: Int -> Integer
menorTriangularConAlMenosNDivisores2 n =
  head [x | x <- triangulares, nDivisores2 x >= n]

-- (nDivisores2 x) es el número de divisores de x. Por ejemplo,
--    nDivisores2 28  ==  6
nDivisores2 :: Integer -> Int
nDivisores2 1 = 1
nDivisores2 n = product [1 + length xs | xs <- group (factoresPrimos n)]

-- (factoresPrimos n) es la lista de los factores primos de n. Por
-- ejemplo,
--    factoresPrimos 28  ==  [2,2,7]
factoresPrimos :: Integer -> [Integer]
factoresPrimos n = aux n primos
  where aux m (p:ps)
          | p*p > m        = [m]
          | m `mod` p == 0 = p : aux (m `div` p) (p:ps)
          | otherwise      = aux m ps

-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : filter ((==1) . length . factoresPrimos) [3,5..]

-- 3ª solución: Factorización usando la lista 'primes' de la librería
-- ==================================================================

menorTriangularConAlMenosNDivisores3 :: Int -> Integer
menorTriangularConAlMenosNDivisores3 n =
  head [x | x <- triangulares, nDivisores3 x >= n]

-- (nDivisores3 x) es el número de divisores de x. Por ejemplo,
--    nDivisores3 28  ==  6
nDivisores3 :: Integer -> Int
nDivisores3 1 = 1
nDivisores3 n = product [1 + length xs | xs <- group (factoresPrimos3 n)]

-- (factoresPrimos3 n) es la lista de los factores primos de n. Por
-- ejemplo,
--    factoresPrimos3 28  ==  [2,2,7]
factoresPrimos3 :: Integer -> [Integer]
factoresPrimos3 n = aux n primes
  where
    aux m (p:ps)
        | p*p > m        = [m]
        | m `mod` p == 0 = p : aux (m `div` p) (p:ps)
        | otherwise      = aux m ps

-- 4ª solución: Usando de 'primeFactors' de la librería
-- ====================================================

menorTriangularConAlMenosNDivisores4 :: Int -> Integer
menorTriangularConAlMenosNDivisores4 n =
  head [x | x <- triangulares, nDivisores4 x >= n]

-- (nDivisores4 x) es el número de divisores de x. Por ejemplo,
--    nDivisores4 28  ==  6
nDivisores4 :: Integer -> Int
nDivisores4 n = product [1 + length xs | xs <- group (primeFactors n)]

-- 5ª solución: Estilo functional (point-free)
-- ===========================================

menorTriangularConAlMenosNDivisores5 :: Int -> Integer
menorTriangularConAlMenosNDivisores5 n =
  head [x | x <- triangulares, nDivisores5 x >= n]

-- (nDivisores5 x) es el número de divisores de x. Por ejemplo,
--    nDivisores5 28  ==  6
nDivisores5 :: Integer -> Int
nDivisores5 =
  product . map ((+1) . length) . group . primeFactors

-- 6ª solución: Optimización matemática
-- ====================================

-- El n-ésimo número triangular es T(n) = n(n+1)2.
--
-- Los números n y n+1 son coprimos (no comparten factores primos). Esto
-- implica que:
-- + Si n es par:   d(T(n)) = d(n/2)⋅d(n+1)
-- + Si n es impar: d(T(n)) = d(n)⋅d((n+1)/2)
-- donde d(x) es el número de divisores de x.

menorTriangularConAlMenosNDivisores6 :: Int -> Integer
menorTriangularConAlMenosNDivisores6 n =
  head [ k*(k+1) `div` 2 | k <- [1..], nDivisoresTriangular k >= n]

nDivisoresTriangular :: Integer -> Int
nDivisoresTriangular k = g k * g (k + 1)
  where
    g i | even i    = nDivisores5 (i `div` 2)
        | otherwise = nDivisores5 i

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Integer) -> Spec
specG menorTriangularConAlMenosNDivisores = do
  it "e1" $
    menorTriangularConAlMenosNDivisores 5  `shouldBe` 28
  it "e2" $
    menorTriangularConAlMenosNDivisores 50 `shouldBe` 25200

spec :: Spec
spec = do
  describe "def. 1" $ specG menorTriangularConAlMenosNDivisores1
  describe "def. 2" $ specG menorTriangularConAlMenosNDivisores2
  describe "def. 3" $ specG menorTriangularConAlMenosNDivisores3
  describe "def. 4" $ specG menorTriangularConAlMenosNDivisores4
  describe "def. 5" $ specG menorTriangularConAlMenosNDivisores5
  describe "def. 6" $ specG menorTriangularConAlMenosNDivisores6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Bool
prop_equivalencia (Positive n) =
  all (== menorTriangularConAlMenosNDivisores1 n)
      [ menorTriangularConAlMenosNDivisores2 n
      , menorTriangularConAlMenosNDivisores3 n
      , menorTriangularConAlMenosNDivisores4 n
      , menorTriangularConAlMenosNDivisores5 n
      , menorTriangularConAlMenosNDivisores6 n
      ]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=50}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia                                        --
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> menorTriangularConAlMenosNDivisores1 100
--    73920
--    (1.69 secs, 952,422,656 bytes)
--    λ> menorTriangularConAlMenosNDivisores2 100
--    73920
--    (0.04 secs, 3,739,104 bytes)
--    λ> menorTriangularConAlMenosNDivisores3 100
--    73920
--    (0.04 secs, 8,445,272 bytes)
--    λ> menorTriangularConAlMenosNDivisores4 100
--    73920
--    (0.03 secs, 7,224,216 bytes)
--    λ> menorTriangularConAlMenosNDivisores5 100
--    73920
--    (0.02 secs, 7,078,136 bytes)
--    λ> menorTriangularConAlMenosNDivisores6 100
--    73920
--    (0.03 secs, 6,139,112 bytes)
--
--    λ> menorTriangularConAlMenosNDivisores2 700
--    236215980
--    (1.26 secs, 755,192,600 bytes)
--    λ> menorTriangularConAlMenosNDivisores3 700
--    236215980
--    (2.30 secs, 3,857,324,424 bytes)
--    λ> menorTriangularConAlMenosNDivisores4 700
--    236215980
--    (1.13 secs, 3,479,164,848 bytes)
--    λ> menorTriangularConAlMenosNDivisores5 700
--    236215980
--    (1.28 secs, 3,475,136,128 bytes)
--    λ> menorTriangularConAlMenosNDivisores6 700
--    236215980
--    (0.48 secs, 1,083,563,856 bytes)
