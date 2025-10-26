-- Parte_impar.hs
-- Parte impar de un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-Noviembre-2014 (actualizado 26-Octubre-2025)
-- =====================================================================

-- ----------------------------------------------------------------------
-- Todo número entero positivo n se puede escribir como 2^k*m, con m
-- impar. Se dice que m es la parte impar de n. Por ejemplo, la parte
-- impar de 40 es 5 porque 40 = 5*2^3.
--
-- Definir la función
--    parteImpar :: Integer -> Integer
-- tal que (parteImpar n) es la parte impar de n. Por ejemplo,
--    parteImpar 40  ==  5
-- ----------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Parte_impar where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

parteImpar1 :: Integer -> Integer
parteImpar1 n | even n    = parteImpar1 (n `div` 2)
              | otherwise = n

-- 2ª solución
-- ===========

parteImpar2 :: Integer -> Integer
parteImpar2 = until odd (`div` 2)

-- 3ª solución
-- ===========

parteImpar3 :: Integer -> Integer
parteImpar3 n =
  n `div` (2 ^ length (takeWhile even (iterate (`div` 2) n)))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG parteImpar = do
  it "e1" $
    parteImpar 40 `shouldBe`  5

spec :: Spec
spec = do
  describe "def. 1" $ specG parteImpar1
  describe "def. 2" $ specG parteImpar2
  describe "def. 3" $ specG parteImpar3

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad de equivalencia es
prop_parteImpar :: Positive Integer -> Bool
prop_parteImpar (Positive n) =
  all (== parteImpar1 n)
      [parteImpar2 n,
       parteImpar3 n]

-- La comprobación es
--    λ> quickCheck prop_parteImpar
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> parteImpar1 (2^300000)
--    1
--    (2.88 secs, 5,708,547,096 bytes)
--    λ> parteImpar2 (2^300000)
--    1
--    (2.82 secs, 5,677,346,992 bytes)
--    λ> parteImpar3 (2^300000)
--    1
--    (2.81 secs, 5,711,063,584 bytes)
