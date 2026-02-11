-- Sumas_de_dos_abundantes.hs
-- Sucesión de sumas de dos números abundantes.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-Febrero-2015 (actualizado 10-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número n es abundante si la suma de los divisores propios de n es
-- mayor que n. El primer número abundante es el 12 (cuyos divisores
-- propios son 1, 2, 3, 4 y 6 cuya suma es 16). Por tanto, el menor
-- número que es la suma de dos números abundantes es el 24.
--
-- Definir la sucesión
--    sumasDeDosAbundantes :: [Integer]
-- cuyos elementos son los números que se pueden escribir como suma de
-- dos números abundantes. Por ejemplo,
--    take 10 sumasDeDosAbundantes     == [24,30,32,36,38,40,42,44,48,50]
--    sumasDeDosAbundantes !! (2*10^4) == 21457
--    sumasDeDosAbundantes !! (10^7)   == 10001457
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sumas_de_dos_abundantes where

import Data.List (genericLength, group)
import Data.Numbers.Primes (primeFactors)
import Math.NumberTheory.ArithmeticFunctions (sigma)
import Data.List.Ordered (unionAll)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumasDeDosAbundantes1 :: [Integer]
sumasDeDosAbundantes1 = [n | n <- [1..], esSumaDeDosAbundantes n]

-- (esSumaDeDosAbundantes n) se verifica si n es suma de dos números
-- abundantes. Por ejemplo,
--    esSumaDeDosAbundantes 24           ==  True
--    any esSumaDeDosAbundantes [1..22]  ==  False
esSumaDeDosAbundantes :: Integer -> Bool
esSumaDeDosAbundantes n = (not . null) [x | x <- xs, n-x `elem` xs]
  where xs = takeWhile (<n) abundantes

-- abundantes es la lista de los números abundantes. Por ejemplo,
--    take 10 abundantes  ==  [12,18,20,24,30,36,40,42,48,54]
abundantes :: [Integer]
abundantes = [n | n <- [2..], abundante n]

-- (abundante n) se verifica si n es abundante. Por ejemplo,
--    abundante 12  ==  True
--    abundante 11  ==  False
abundante :: Integer -> Bool
abundante n = sum (divisores n) > n

-- (divisores n) es la lista de los divisores propios de n. Por ejemplo,
--    divisores 12  ==  [1,2,3,4,6]
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n `div` 2], n `mod` x == 0]

-- 2ª solución
-- ===========

sumasDeDosAbundantes2 :: [Integer]
sumasDeDosAbundantes2 = filter esSumaDeDosAbundantes2 [1..]

esSumaDeDosAbundantes2 :: Integer -> Bool
esSumaDeDosAbundantes2 n = (not . null) [x | x <- xs, n-x `elem` xs]
  where xs = takeWhile (<n) abundantes2

abundantes2 :: [Integer]
abundantes2 = filter abundante2 [2..]

abundante2 :: Integer -> Bool
abundante2 n = sumaDivisores n > n

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

sumasDeDosAbundantes3 :: [Integer]
sumasDeDosAbundantes3 = filter esSumaDeDosAbundantes3 [1..]

esSumaDeDosAbundantes3 :: Integer -> Bool
esSumaDeDosAbundantes3 n = (not . null) [x | x <- xs, n-x `elem` xs]
  where xs = takeWhile (<n) abundantes3

abundantes3 :: [Integer]
abundantes3 = filter abundante3 [3..]

abundante3 :: Integer -> Bool
abundante3 n = n < sumaDivisores3 n - n

sumaDivisores3 :: Integer -> Integer
sumaDivisores3 = sigma 1

-- 4ª definición
-- =============

sumasDeDosAbundantes4 :: [Integer]
sumasDeDosAbundantes4 =
  unionAll [ [a + b | b <- dropWhile (<a) abundantes3]
           | a <- abundantes3 ]

-- 5ª definición
-- =============

sumasDeDosAbundantes5 :: [Integer]
sumasDeDosAbundantes5 =
  takeWhile (<20161) sumasDeDosAbundantes4 ++ [20162..]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG sumasDeDosAbundantes = do
  it "e1" $
    take 10 sumasDeDosAbundantes `shouldBe` [24,30,32,36,38,40,42,44,48,50]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumasDeDosAbundantes1
  describe "def. 2" $ specG sumasDeDosAbundantes2
  describe "def. 3" $ specG sumasDeDosAbundantes3
  describe "def. 4" $ specG sumasDeDosAbundantes4
  describe "def. 5" $ specG sumasDeDosAbundantes5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Bool
prop_equivalencia (Positive n) =
  all (== sumasDeDosAbundantes1 !! n)
      [ sumasDeDosAbundantes2 !! n
      , sumasDeDosAbundantes3 !! n
      , sumasDeDosAbundantes4 !! n
      , sumasDeDosAbundantes5 !! n
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumasDeDosAbundantes1 !! (2*10^3)
--    2887
--    (2.60 secs, 515,985,136 bytes)
--    λ> sumasDeDosAbundantes2 !! (2*10^3)
--    2887
--    (1.80 secs, 141,515,008 bytes)
--    λ> sumasDeDosAbundantes3 !! (2*10^3)
--    2887
--    (1.70 secs, 99,952,000 bytes)
--    λ> sumasDeDosAbundantes4 !! (2*10^3)
--    2887
--    (0.14 secs, 42,338,832 bytes)
--    λ> sumasDeDosAbundantes5 !! (2*10^3)
--    2887
--    (0.14 secs, 43,307,904 bytes)
--
--    λ> sumasDeDosAbundantes4 !! (3*10^4)
--    31457
--    (8.54 secs, 3,964,578,888 bytes)
--    λ> :r
--    Ok, one module loaded.
--    λ> sumasDeDosAbundantes5 !! (3*10^4)
--    31457
--    (4.44 secs, 1,688,872,056 bytes)

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + A048260: The sum of 2 (not necessarily distinct) abundant numbers.
--   https://oeis.org/A048260
--
-- + E23: Non-abundant sums. Euler Project http://bit.ly/1A1vScr
