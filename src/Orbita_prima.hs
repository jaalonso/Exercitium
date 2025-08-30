-- Orbita_prima.hs
-- Órbita prima.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-Junio-2014 (actualizado 30-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La órbita prima de un número n es la sucesión construida de la
-- siguiente forma:
-- + si n es compuesto su órbita no tiene elementos
-- + si n es primo, entonces n está en su órbita; además, sumamos n y
--   sus dígitos, si el resultado es un número primo repetimos el
--   proceso hasta obtener un número compuesto.
--
-- Por ejemplo, con el 11 podemos repetir el proceso dos veces
--    13 = 11+1+1
--    17 = 13+1+3
--    25 = 17+1+7
-- Así, la órbita prima de 11 es 11, 13, 17.
--
-- Definir la función
--    orbita :: Integer -> [Integer]
-- tal que (orbita n) es la órbita prima de n. Por ejemplo,
--    orbita 11 == [11,13,17]
--    orbita 59 == [59,73,83]
--
-- Calcular el menor número cuya órbita prima tiene más de 3 elementos.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Orbita_prima where

import Data.Numbers.Primes (isPrime)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

orbita1 :: Integer -> [Integer]
orbita1 n | not (esPrimo n) = []
          | otherwise       = n : orbita1 (n + sum (digitos n))

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 7   ==  True
--    esPrimo 15  ==  False
esPrimo :: Integer -> Bool
esPrimo n = [x | x <- [1..n], n `rem` x == 0] == [1,n]

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 15  ==  [1,5]
digitos :: Integer -> [Integer]
digitos n = [read [x]| x <- show n]

-- 2ª solución
-- ===========

orbita2 :: Integer -> [Integer]
orbita2 n = takeWhile esPrimo (iterate f n)
  where f x = x + sum (digitos x)

-- 3ª solución
-- ===========

orbita3 :: Integer -> [Integer]
orbita3 n = takeWhile isPrime (iterate f n)
  where f x = x + sum (digitos x)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG orbita = do
  it "e1" $
    orbita 11 `shouldBe` [11,13,17]
  it "e2" $
    orbita 59 `shouldBe` [59,73,83]

spec :: Spec
spec = do
  describe "def. 1"  $ specG orbita1
  describe "def. 2"  $ specG orbita2
  describe "def. 3"  $ specG orbita3

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_orbita :: Integer -> Bool
prop_orbita n =
  all (== orbita1 n)
      [orbita2 n,
       orbita3 n]

-- La comprobación es
--    λ> quickCheck prop_orbita
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> orbita1 516493
--    [516493,516521,516541,516563,516589,516623]
--    (1.12 secs, 620,631,504 bytes)
--    λ> orbita2 516493
--    [516493,516521,516541,516563,516589,516623]
--    (1.08 secs, 620,631,112 bytes)
--    λ> orbita3 516493
--    [516493,516521,516541,516563,516589,516623]
--    (0.01 secs, 2,340,960 bytes)

-- Cálculo
-- =======

-- El cálculo es
--    λ> head [x | x <- [1,3..], length (orbita3 x) > 3]
--    277
--
--    λ> orbita3 277
--    [277,293,307,317]
