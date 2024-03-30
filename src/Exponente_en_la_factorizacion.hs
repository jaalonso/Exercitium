-- Exponente_en_la_factorizacion.hs
-- Exponente en la factorización.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-marzo-24
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    exponente :: Integer -> Integer -> Int
-- tal que (exponente x n) es el exponente de x en la factorización
-- prima de n (se supone que x > 1 y n > 0). Por ejemplo,
--    exponente 2 24  ==  3
--    exponente 3 24  ==  1
--    exponente 6 24  ==  0
--    exponente 7 24  ==  0
-- ---------------------------------------------------------------------

module Exponente_en_la_factorizacion where

import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

exponente1 :: Integer -> Integer -> Int
exponente1 x n
  | esPrimo x = aux n
  | otherwise = 0
  where aux m | m `mod` x == 0 = 1 + aux (m `div` x)
              | otherwise      = 0

-- (esPrimo x) se verifica si x es un número primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 8  ==  False
esPrimo :: Integer -> Bool
esPrimo x =
  [y | y <- [1..x], x `mod` y == 0] == [1,x]

-- 2ª solución
-- ===========

exponente2 :: Integer -> Integer -> Int
exponente2 x n
  | esPrimo x = length (takeWhile (`divisible` x) (iterate (`div` x) n))
  | otherwise = 0

-- (divisible n x) se verifica si n es divisible por x. Por ejemplo,
--    divisible 6 2  ==  True
--    divisible 7 2  ==  False
divisible :: Integer -> Integer -> Bool
divisible n x = n `mod` x == 0

-- 3ª solución
-- ===========

exponente3 :: Integer -> Integer -> Int
exponente3 x n =
  length (filter (==x) (primeFactors n))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer -> Int) -> Spec
specG exponente = do
  it "e1" $
    exponente 2 24  `shouldBe`  3
  it "e2" $
    exponente 3 24  `shouldBe`  1
  it "e3" $
    exponente 6 24  `shouldBe`  0
  it "e4" $
    exponente 7 24  `shouldBe`  0

spec :: Spec
spec = do
  describe "def. 1" $ specG exponente1
  describe "def. 2" $ specG exponente2
  describe "def. 3" $ specG exponente3

-- La verificación es
--    λ> verifica
--
--    12 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_exponente :: Integer -> Integer -> Property
prop_exponente x n =
  x > 1 && n > 0 ==>
  exponente1 x n == exponente2 x n &&
  exponente1 x n == exponente3 x n

-- La comprobación es
--    λ> quickCheck prop_exponente
--    +++ OK, passed 100 tests.
