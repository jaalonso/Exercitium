-- Numeros_pitagoricos.hs
-- 2015 y los números pitagóricos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-Enero-2015 (actualizado 24-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número pitagórico es un número natural cuyo cuadrado se puede
-- escribir como la suma de los cuadrados de dos números naturales no
-- nulos; es decir, el número natural a es pitagórico si existen dos
-- números naturales b y c distintos de cero tales que a² = b²+c². Por
-- ejemplo, 5 es un número pitagórico ya que 5² = 3²+4² y también lo es
-- 2015 ya que 2015² = 1612²+1209².
--
-- Definir la sucesión
--     pitagoricos :: [Integer]
-- cuyos elementos son los números pitagóricos. Por ejemplo,
--    λ> take 20 pitagoricos
--    [5,10,13,15,17,20,25,26,29,30,34,35,37,39,40,41,45,50,51,52]
--    λ> pitagoricos !! 50000
--    73035
--
-- Calcular la posición de 2015 en la sucesión.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Numeros_pitagoricos where

import Data.Numbers.Primes
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

pitagoricos1 :: [Integer]
pitagoricos1 = [n | n <- [1..], esPitagorico1 n]

-- (esPitagorico n) se verifica si n es pitagórico. Por ejemplo,
--    esPitagorico 5  ==  True
--    esPitagorico 6  ==  False
esPitagorico1 :: Integer -> Bool
esPitagorico1 n = not (null (ternasPitagoricas n))

-- (ternasPitagoricas n) es la lista de las ternas (n,a,b) tales que
-- n² = a²+b² con b < a < n. Por ejemplo,
--    λ> ternasPitagoricas 5
--    [(5,4,3)]
--    λ> ternasPitagoricas 2015
--    [(2015,1612,1209),(2015,1736,1023),(2015,1860,775),(2015,1953,496)]
ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas n =
  [(n,a,b) | a <- [1..n-1],
             let b2 = n^2-a^2,
             let b = round (sqrt (fromIntegral b2)),
             b < a,
             b^2 == b2]

-- 2ª solución
-- ===========

pitagoricos2 :: [Integer]
pitagoricos2 = [n | n <- [1..], esPitagorico2 n]

esPitagorico2 :: Integer -> Bool
esPitagorico2 n = or [esCuadrado2 (n^2 - b^2) | b <- [1..n-1]]

esCuadrado2 :: Integer -> Bool
esCuadrado2 n =
  m * m == n
  where m = round (sqrt (fromIntegral n))

-- 3ª solución
-- ===========

pitagoricos3 :: [Integer]
pitagoricos3 = [round (sqrt (fromIntegral a)) |
                a <- cuadrados,
                let xs = takeWhile (<a) cuadrados,
                any (\b -> a-b `elem` xs) xs]

-- cuadrados es la lista de los cuadrados de los números naturales. Por
-- ejemplo,
--    take 10 cuadrados  ==  [1,4,9,16,25,36,49,64,81,100]
cuadrados :: [Integer]
cuadrados = [n*n | n <- [1..]]

-- 4ª solución
-- ===========

pitagoricos4 :: [Integer]
pitagoricos4 = [n | n <- [1..], esPitagorico4 n]

esPitagorico4 :: Integer -> Bool
esPitagorico4 n = or [esCuadrado4 (n^2 - b^2) | b <- [1..n-1]]

esCuadrado4 :: Integer -> Bool
esCuadrado4 n =
  -- Un cuadrado perfecto mod 16 solo puede ser 0, 1, 4 o 9
  (n `rem` 16) `elem` [0, 1, 4, 9] && m * m == n
  where
    m   = round (sqrt (fromIntegral n))

-- 5ª solución
-- ===========

pitagoricos5 :: [Integer]
pitagoricos5 = [n | n <- [1..], esPitagorico5 n]

-- Basado en la una propiedad: un número natural es la hipotenusa de un
-- triángulo rectángulo (es un número pitagórico) si y sólo si tiene al
-- menos un factor primo de la forma 4k+1.
esPitagorico5 :: Integer -> Bool
esPitagorico5 n =
  or [x `mod` 4 == 1 | x <- takeWhile (<=n) primes, n `mod` x == 0]

-- 6ª solución
-- ===========

pitagoricos6 :: [Integer]
pitagoricos6 = [n | n <- [1..], esPitagorico6 n]

esPitagorico6 :: Integer -> Bool
esPitagorico6 n = tieneFactor4kMas1 n primes
  where
    tieneFactor4kMas1 1 _ = False
    tieneFactor4kMas1 x (p:ps)
      | p * p > x      = x `mod` 4 == 1
      | x `mod` p == 0 = (p `mod` 4 == 1) || tieneFactor4kMas1 (reducir x p) ps
      | otherwise      = tieneFactor4kMas1 x ps
    reducir x p = if x `mod` p == 0 then reducir (x `div` p) p else x

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG pitagoricos = do
  it "e1" $
    take 20 pitagoricos `shouldBe`
    [5,10,13,15,17,20,25,26,29,30,34,35,37,39,40,41,45,50,51,52]

spec :: Spec
spec = do
  describe "def. 1" $ specG pitagoricos1
  describe "def. 2" $ specG pitagoricos2
  describe "def. 3" $ specG pitagoricos3
  describe "def. 4" $ specG pitagoricos4
  describe "def. 5" $ specG pitagoricos5
  describe "def. 6" $ specG pitagoricos6

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
  all (== pitagoricos1 !! n)
      [pitagoricos2 !! n,
       pitagoricos3 !! n,
       pitagoricos4 !! n,
       pitagoricos5 !! n,
       pitagoricos6 !! n]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> pitagoricos1 !! 1000
--    1700
--    (2.19 secs, 2,481,911,064 bytes)
--    λ> pitagoricos2 !! 1000
--    1700
--    (1.63 secs, 1,820,269,800 bytes)
--    λ> pitagoricos3 !! 1000
--    1700
--    (10.16 secs, 219,367,560 bytes)
--    λ> pitagoricos4 !! 1000
--    1700
--    (1.82 secs, 1,883,995,880 bytes)
--    λ> pitagoricos5 !! 1000
--    1700
--    (0.19 secs, 278,054,352 bytes)
--    λ> pitagoricos6 !! 1000
--    1700
--    (0.08 secs, 23,122,144 bytes)
--
--    λ> pitagoricos5 !! 5000
--    7832
--    (2.25 secs, 4,451,130,264 bytes)
--    λ> pitagoricos6 !! 5000
--    7832
--    (0.19 secs, 156,842,800 bytes)

-- Cálculo
-- =======

-- El cálculo de la posición de 2015 es
--    λ> length (takeWhile (<2015) pitagoricos)
--    1195

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- La sucesión pitagoricos es la A009003 (https://oeis.org/A009003) de
-- la OEIS.
