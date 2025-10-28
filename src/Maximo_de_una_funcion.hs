-- Maximo_de_una_funcion.hs
-- Máximo de una función
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-Noviembre-2014 (actualizado 28-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se considera la siguiente función
--    g :: Integer -> Integer
--    g n = if n < 10 then n*n else n
--
-- Definir la función
--    maxG :: Integer -> Integer
-- tal que (maxG n) es el punto i del intervalo [0,n] donde g alcanza
-- el máximo de sus valores, si n es positivo y 0 en caso contrario. Por
-- ejemplo,
--    maxG (-7)  ==  0
--    maxG 7     ==  7
--    maxG 14    ==  9
--    maxG 84    ==  84
--
-- Comprobar con QuickCheck que la función maxG es equivalente a la
-- función f definida por
--    f :: Integer -> Integer
--    f n | n < 0             = 0
--        | n >= 10 && n < 81 = 9
--        | otherwise         = n
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Maximo_de_una_funcion where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

g :: Integer -> Integer
g n = if n < 10 then n*n else n

-- 1ª solución
-- ===========

maxG1 :: Integer -> Integer
maxG1 n | n < 0     = 0
        | otherwise = snd (maximum [(g i,i) | i <- [0..n]])

-- 2ª solución
-- ===========

maxG2 :: Integer -> Integer
maxG2 n | n < 0      = 0
        | g m > g n  = m
        | otherwise  = n
  where m = maxG2 (n - 1)


-- 3ª solución
-- ===========

maxG3 :: Integer -> Integer
maxG3 n
  | n <= 0    = 0
  | n <= 9    = n
  | otherwise = if g 9 > n then 9 else n

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG maxG = do
  it "e1" $
    maxG (-7)  `shouldBe`  0
  it "e2" $
    maxG 7     `shouldBe`  7
  it "e3" $
    maxG 14    `shouldBe`  9
  it "e4" $
    maxG 84    `shouldBe`  84

spec :: Spec
spec = do
  describe "def. 1" $ specG maxG1
  describe "def. 2" $ specG maxG2
  describe "def. 3" $ specG maxG3

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_maxG :: Integer -> Bool
prop_maxG n =
  all (== maxG1 n)
      [maxG2 n,
       maxG3 n]

-- La comprobación es
--    λ> quickCheck prop_maxG
--    +++ OK, passed 100 tests.

-- Comprobación de la propiedad
-- ============================

f :: Integer -> Integer
f n | n < 0             = 0
    | n >= 10 && n < 81 = 9
    | otherwise         = n

-- La propiedad es
prop_max2 :: Integer -> Bool
prop_max2 n =
  maxG1 n == f n

-- La comprobación es
--    λ> quickCheck prop_max2
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maxG1 (2*10^6)
--    2000000
--    (1.01 secs, 528,602,360 bytes)
--    λ> maxG2 (2*10^6)
--    2000000
--    (2.54 secs, 1,266,852,576 bytes)
--    λ> maxG3 (2*10^6)
--    2000000
--    (0.01 secs, 601,672 bytes)
--    λ> f (2*10^6)
--    2000000
--    (0.01 secs, 601,528 bytes)
