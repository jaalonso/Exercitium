-- Sumas_de_4_primos.hs
-- Sumas de 4 primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-Febrero-2015 (actualizado 11-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La conjetura de Waring sobre los números primos establece que todo
-- número impar es primo o la suma de tres primos. La conjetura de
-- Goldbach afirma que todo número par mayor que 2 es la suma de dos
-- números primos. Ambos problemas ha estado abiertos durante más de 200
-- años. En este problema no se propone su solución, sino una tarea más
-- simple: buscar una manera de expresar los enteros mayores que 7 como
-- suma de exactamente cuatro números primos; es decir, definir la
-- función
--    suma4primos :: Integer -> (Integer,Integer,Integer,Integer)
-- tal que (suma4primos n) es una cuádrupla (a,b,c,d) de números primos
-- cuya suma es n (que se supone mayor que 7). Por ejemplo,
--    suma4primos 24             ==  (2,2,3,17)
--    suma4primos 1234567890123  ==  (2,3,29,1234567890089)
-- Nota: Para cada n pueden existir distintas cuádruplas que cumplan la
-- especificación. Por ejemplo, para el 16 hay tres: (2,2,5,7),
-- (3,3,3,7) y (3,3,5,5). Cualquiera de ellas se admite como solución.
--
-- Comprobar con QuickCheck que suma4primos es correcta; es decir si
-- (suma4primos n) es (a,b,c,d) entonces los números a, b c y d son
-- primos y su suma es n.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sumas_de_4_primos where

import Data.Numbers.Primes (isPrime, primes)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Fuerza bruta exhaustiva
-- ====================================

suma4primos1 :: Integer -> (Integer,Integer,Integer,Integer)
suma4primos1 = head . sumas4primos

sumas4primos :: Integer -> [(Integer,Integer,Integer,Integer)]
sumas4primos n =
  [(a,b,c,d)
  | let as = takeWhile (<n) primes,
    a <- as,
    let bs = takeWhile (<n-a) (dropWhile (<a) as),
    b <- bs,
    let cs = takeWhile (<n-a-b) (dropWhile (<b) bs),
    c <- cs,
    let d = n-a-b-c,
    d `elem` dropWhile (<c) cs]

-- 2ª solución: Basada en Goldbach usando listas de primos precalculadas
-- =====================================================================

suma4primos2 :: Integer -> (Integer,Integer,Integer,Integer)
suma4primos2 n | even n    = (2,2,a,b)
               | otherwise = (2,3,c,d)
  where (a,b) = head (sumas2primos (n-4))
        (c,d) = head (sumas2primos (n-5))

sumas2primos :: Integer -> [(Integer,Integer)]
sumas2primos n = [(x,y)
                 | x <- takeWhile (<n) primes,
                   let y = n-x,
                   x <= y,
                   isPrime y]

-- 3ª solución: Basada en Goldbach con búsqueda perezosa y aritmética directa
-- ==========================================================================

suma4primos3 :: Integer -> (Integer,Integer,Integer,Integer)
suma4primos3 n | even n    = (2,2,a,b)
               | otherwise = (2,3,c,d)
  where (a,b) = suma2primos (n-4)
        (c,d) = suma2primos (n-5)

suma2primos :: Integer -> (Integer,Integer)
suma2primos n
  | n == 4    = (2, 2)
  | otherwise = head [(p, n - p) | p <- [3, 5 .. n `div` 2], isPrime p, isPrime (n - p)]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> (Integer,Integer,Integer,Integer)) -> Spec
specG suma4primos = do
  it "e1" $
    suma4primos 24 `shouldBe` (2,2,3,17)

spec :: Spec
spec = do
  describe "def. 1" $ specG suma4primos1
  describe "def. 2" $ specG suma4primos2
  describe "def. 3" $ specG suma4primos3

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> suma4primos1 209507
--    (2,3,5,209497)
--    (16.59 secs, 75,450,464 bytes)
--    λ> :r
--    Ok, one module loaded.
--    λ> suma4primos2 209507
--    (2,3,5,209497)
--    (0.02 secs, 808,288 bytes)
--    λ> suma4primos3 209507
--    (2,3,5,209497)
--    (0.01 secs, 807,064 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_suma4primos :: (Integer -> (Integer,Integer,Integer,Integer)) -> Property
prop_suma4primos f =
  forAll (choose (8, 1000)) $ \n ->
    let (a, b, c, d) = f n
    in all isPrime [a, b, c, d] && (a + b + c + d == n)

-- La comprobación es
--    λ> quickCheck (prop_suma4primos suma4primos1)
--    +++ OK, passed 100 tests.
--    λ> quickCheck (prop_suma4primos suma4primos2)
--    +++ OK, passed 100 tests.
--    λ> quickCheck (prop_suma4primos suma4primos3)
--    +++ OK, passed 100 tests.
