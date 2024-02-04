-- La_funcion_indicatriz_de_Euler.hs
-- La función indicatriz de Euler.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-enero-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La [indicatriz de Euler](https://bit.ly/3yQbzA6) (también llamada
-- función φ de Euler) es una función importante en teoría de
-- números. Si n es un entero positivo, entonces φ(n) se define como el
-- número de enteros positivos menores o iguales a n y coprimos con
-- n. Por ejemplo, φ(36) = 12 ya que los números menores o iguales a 36
-- y coprimos con 36 son doce: 1, 5, 7, 11, 13, 17, 19, 23, 25, 29, 31,
-- y 35.
--
-- Definir la función
--    phi :: Integer -> Integer
-- tal que (phi n) es igual a φ(n). Por ejemplo,
--    phi 36                          ==  12
--    map phi [10..20]                ==  [4,10,4,12,6,8,8,16,6,18,8]
--    phi (3^10^5) `mod` (10^9)       ==  681333334
--    length (show (phi (10^(10^5)))) == 100000
--
-- Comprobar con QuickCheck que, para todo n > 0, φ(10^n) tiene n
-- dígitos.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module La_funcion_indicatriz_de_Euler where

import Data.List (genericLength, group)
import Data.Numbers.Primes (primeFactors)
import Math.NumberTheory.ArithmeticFunctions (totient)
import Test.QuickCheck (Positive (Positive), quickCheck)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

phi1 :: Integer -> Integer
phi1 n = genericLength [x | x <- [1..n], gcd x n == 1]

-- 2ª solución
-- ===========

phi2 :: Integer -> Integer
phi2 n = product [(p-1)*p^(e-1) | (p,e) <- factorizacion n]

factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n =
  [(head xs,genericLength xs) | xs <- group (primeFactors n)]

-- 3ª solución
-- =============

phi3 :: Integer -> Integer
phi3 n =
  product [(x-1) * product xs | (x:xs) <- group (primeFactors n)]

-- 4ª solución
-- =============

phi4 :: Integer -> Integer
phi4 = totient

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG phi = do
  it "e1" $
    phi 36 `shouldBe` 12
  it "e2" $
    map phi [10..20] `shouldBe` [4,10,4,12,6,8,8,16,6,18,8]

spec :: Spec
spec = do
  describe "def. 1" $ specG phi1
  describe "def. 2" $ specG phi2
  describe "def. 3" $ specG phi3
  describe "def. 4" $ specG phi4

-- La verificación es
--    λ> verifica
--
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_phi :: Positive Integer -> Bool
prop_phi (Positive n) =
  all (== phi1 n)
      [phi2 n,
       phi3 n,
       phi4 n]

-- La comprobación es
--    λ> quickCheck prop_phi
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> phi1 (2*10^6)
--    800000
--    (2.49 secs, 2,117,853,856 bytes)
--    λ> phi2 (2*10^6)
--    800000
--    (0.02 secs, 565,664 bytes)
--
--    λ> length (show (phi2 (10^100000)))
--    100000
--    (2.80 secs, 5,110,043,208 bytes)
--    λ> length (show (phi3 (10^100000)))
--    100000
--    (4.81 secs, 7,249,353,896 bytes)
--    λ> length (show (phi4 (10^100000)))
--    100000
--    (0.78 secs, 1,467,573,768 bytes)

-- Verificación de la propiedad
-- ============================

-- La propiedad es
prop_phi2 :: Positive Integer -> Bool
prop_phi2 (Positive n) =
  genericLength (show (phi4 (10^n))) == n

-- La comprobación es
--    λ> quickCheck prop_phi2
--    +++ OK, passed 100 tests.
