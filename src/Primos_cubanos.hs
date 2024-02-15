-- Primos_cubanos.hs
-- Primos cubanos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-febrero-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un [primo cubano](http://bit.ly/1jPy5QZ) es un número primo que se
-- puede escribir como diferencia de dos cubos consecutivos. Por
-- ejemplo, el 61 es un primo cubano porque es primo y 61 = 5³-4³.
--
-- Definir la sucesión
--    cubanos :: [Integer]
-- tal que sus elementos son los números cubanos. Por ejemplo,
--    λ> take 15 cubanos
--    [7,19,37,61,127,271,331,397,547,631,919,1657,1801,1951,2269]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Primos_cubanos where

import Data.Numbers.Primes (isPrime)
import Test.QuickCheck
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

cubanos1 :: [Integer]
cubanos1 = filter isPrime (zipWith (-) (tail cubos) cubos)

-- cubos es la lista de los cubos. Por ejemplo,
--    λ> take 10 cubos
--    [1,8,27,64,125,216,343,512,729,1000]
cubos :: [Integer]
cubos = map (^3) [1..]

-- 2ª solución
-- ===========

cubanos2 :: [Integer]
cubanos2 = filter isPrime [(x+1)^3 - x^3 | x <- [1..]]

-- 3ª solución
-- ===========

cubanos3 :: [Integer]
cubanos3 = filter isPrime [3*x^2 + 3*x + 1 | x <- [1..]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG cubanos = do
  it "e1" $
    take 15 cubanos `shouldBe`
    [7,19,37,61,127,271,331,397,547,631,919,1657,1801,1951,2269]

spec :: Spec
spec = do
  describe "def. 1" $ specG cubanos1
  describe "def. 2" $ specG cubanos2
  describe "def. 3" $ specG cubanos3

-- La verificación es
--    λ> verifica
--
--    3 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_cubanos :: NonNegative Int -> Bool
prop_cubanos (NonNegative n) =
  all (== cubanos1 !! n)
      [cubanos2 !! n,
       cubanos3 !! n]

-- La comprobación es
--    λ> quickCheck prop_cubanos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> cubanos1 !! 3000
--    795066361
--    (4.21 secs, 16,953,612,192 bytes)
--    λ> cubanos2 !! 3000
--    795066361
--    (4.27 secs, 16,962,597,288 bytes)
--    λ> cubanos3 !! 3000
--    795066361
--    (4.29 secs, 16,956,085,672 bytes)
