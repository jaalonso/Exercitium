-- Suma_alterna_de_factoriales.hs
-- Sumas alternas de factoriales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las primeras sumas alternas de los factoriales son números primos; en
-- efecto, 
--    3! - 2! + 1! = 5
--    4! - 3! + 2! - 1! = 19
--    5! - 4! + 3! - 2! + 1! = 101
--    6! - 5! + 4! - 3! + 2! - 1! = 619
--    7! - 6! + 5! - 4! + 3! - 2! + 1! = 4421
--    8! - 7! + 6! - 5! + 4! - 3! + 2! - 1! = 35899
-- son primos, pero
--    9! - 8! + 7! - 6! + 5! - 4! + 3! - 2! + 1! = 326981
-- no es primo.
--
-- Definir las funciones
--    sumaAlterna         :: Integer -> Integer
--    sumasAlternas       :: [Integer]
--    conSumaAlternaPrima :: [Integer]
-- tales que
-- + (sumaAlterna n) es la suma alterna de los factoriales desde n hasta
--   1. Por ejemplo,
--      sumaAlterna 3  ==  5
--      sumaAlterna 4  ==  19
--      sumaAlterna 5  ==  101
--      sumaAlterna 6  ==  619
--      sumaAlterna 7  ==  4421
--      sumaAlterna 8  ==  35899
--      sumaAlterna 9  ==  326981
--      sumaAlterna (5*10^4) `mod` (10^6) == 577019
-- + sumasAlternas es la sucesión de las sumas alternas de factoriales. 
--   Por ejemplo, 
--      λ> take 10 sumasAlternas1
--      [0,1,1,5,19,101,619,4421,35899,326981]
-- + conSumaAlternaPrima es la sucesión de los números cuya suma alterna
--   de factoriales es prima. Por ejemplo, 
--      λ> take 8 conSumaAlternaPrima
--      [3,4,5,6,7,8,10,15]
-- ---------------------------------------------------------------------

module Suma_alterna_de_factoriales where

import Data.List (genericTake)
import Data.Numbers.Primes (isPrime)
import Test.QuickCheck

-- 1ª definición de sumaAlterna
-- ============================

sumaAlterna1 :: Integer -> Integer
sumaAlterna1 1 = 1
sumaAlterna1 n = factorial n - sumaAlterna1 (n-1)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- 2ª definición de sumaAlterna
-- ============================

sumaAlterna2 :: Integer -> Integer
sumaAlterna2 n =
  sum (genericTake n (zipWith (*) signos (tail factoriales)))
  where
    signos | odd n     = concat (repeat [1,-1])
           | otherwise = concat (repeat [-1,1])

-- factoriales es la lista de los factoriales. Por ejemplo,
--    take 7 factoriales  ==  [1,1,2,6,24,120,720]
factoriales :: [Integer]
factoriales = 1 : scanl1 (*) [1..]

-- 3ª definición de sumaAlterna
-- ============================

sumaAlterna3 :: Integer -> Integer
sumaAlterna3 n = 
  sum (genericTake n (zipWith (*) signos (tail factoriales)))
  where signos | odd n     = cycle [1,-1]
               | otherwise = cycle [-1,1]

-- 3ª definición de sumaAlterna
-- ============================

sumaAlterna4 :: Integer -> Integer
sumaAlterna4 n =
  foldl (flip (-)) 0 (scanl1 (*) [1..n])

-- Comprobación de equivalencia de sumaAlterna
-- ===========================================

-- La propiedad es
prop_sumaAlterna :: Positive Integer -> Bool 
prop_sumaAlterna (Positive n) =
  all (== sumaAlterna1 n)
      [sumaAlterna2 n,
       sumaAlterna3 n,
       sumaAlterna4 n]

-- La comprobación es
--    λ> quickCheck prop_sumaAlterna
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de sumaAlterna 
-- ========================================

-- La comparación es
--    λ> sumaAlterna1 4000 `mod` (10^6)
--    577019
--    (6.21 secs, 16,154,113,192 bytes)
--    λ> sumaAlterna2 4000 `mod` (10^6)
--    577019
--    (0.01 secs, 24,844,664 bytes)
--    
--    λ> sumaAlterna2 (5*10^4) `mod` (10^6)
--    577019
--    (1.81 secs, 4,729,583,864 bytes)
--    λ> sumaAlterna3 (5*10^4) `mod` (10^6)
--    577019
--    (0.89 secs, 4,725,983,928 bytes)
--    λ> sumaAlterna4 (5*10^4) `mod` (10^6)
--    577019
--    (0.70 secs, 4,710,770,592 bytes)

-- En lo que sigue se usa la 3ª definición
sumaAlterna :: Integer -> Integer
sumaAlterna = sumaAlterna3

-- 1ª definición de sumasAlternas
-- ==============================

sumasAlternas1 :: [Integer]
sumasAlternas1 =
  map sumaAlterna [0..]

-- 2ª definición de sumasAlternas
-- ==============================

sumasAlternas2 :: [Integer]
sumasAlternas2 =
  0 : zipWith (-) (tail factoriales) sumasAlternas2

-- 3ª definición de sumasAlternas
-- ==============================

sumasAlternas3 :: [Integer]
sumasAlternas3 =
  scanl (flip (-)) 0 $ scanl1 (*) [1..]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumasAlternas :: NonNegative Int -> Bool
prop_sumasAlternas (NonNegative n) =
  all (== sumasAlternas1 !! n)
      [sumasAlternas2 !! n,
       sumasAlternas3 !! n]
  
-- La comprobación es
--    λ> quickCheck prop_sumasAlternas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (sumasAlternas1 !! (5*10^4)))
--    213237
--    (4.90 secs, 4,731,620,600 bytes)
--    λ> length (show (sumasAlternas2 !! (5*10^4)))
--    213237
--    (2.39 secs, 4,726,820,456 bytes)
--    λ> length (show (sumasAlternas3 !! (5*10^4)))
--    213237
--    (1.78 secs, 4,726,820,280 bytes)

-- 1ª definición de conSumaAlternaPrima
-- ====================================

conSumaAlternaPrima1 :: [Integer]
conSumaAlternaPrima1 =
  [n | n <- [0..], isPrime (sumaAlterna n)]

-- 2ª definición de conSumaAlternaPrima
-- ====================================

conSumaAlternaPrima2 :: [Integer]
conSumaAlternaPrima2 =
  [x | (x,y) <- zip [0..] sumasAlternas2, isPrime y]

-- 3ª definición de conSumaAlternaPrima
-- ====================================

conSumaAlternaPrima3 :: [Integer]
conSumaAlternaPrima3 =
  filter (isPrime . sumaAlterna) [0..]

-- Comprobación de equivalencia de conSumaAlternaPrima
-- ===================================================

-- La propiedad es
prop_conSumaAlternaPrima :: NonNegative Int -> Bool
prop_conSumaAlternaPrima (NonNegative n) =
  all (== conSumaAlternaPrima1 !! n)
      [conSumaAlternaPrima2 !! n,
       conSumaAlternaPrima3 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=5}) prop_conSumaAlternaPrima
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- + J.D. Cook. [Alternating sums of factorials](http://bit.ly/1Yd8sXR).
-- + OEIS [A001272](http://oeis.org/A001272).
-- + OEIS [A005165](http://oeis.org/A005165).

