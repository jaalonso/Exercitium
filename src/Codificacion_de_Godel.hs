-- Codificacion_de_Godel.hs
-- Codificación de Gödel.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dada una lista de números naturales xs, la
-- [codificación de Gödel](http://bit.ly/2FOzmW1) de xs se obtiene
-- multiplicando las potencias de los primos sucesivos, 
-- siendo los exponentes los sucesores de los elementos de xs. Por
-- ejemplo, si xs = [6,0,4], la codificación de xs es  
--    2^7 * 3^1 * 5^5 = 1200000
-- 
-- Definir las funciones
--    codificaG   :: [Integer] -> Integer
--    decodificaG :: Integer -> [Integer]
-- tales que 
-- + (codificaG xs) es la codificación de Gödel de xs. Por ejemplo, 
--      codificaG [6,0,4]            ==  1200000
--      codificaG [3,1,1]            ==  3600
--      codificaG [3,1,0,0,0,0,0,1]  ==  4423058640
--      codificaG [1..6]             ==  126111168580452537982500
-- + (decodificaG n) es la lista xs cuya codificación es n. Por ejemplo,
--      decodificaG 1200000                   ==  [6,0,4]
--      decodificaG 3600                      ==  [3,1,1]
--      decodificaG 4423058640                ==  [3,1,0,0,0,0,0,1]
--      decodificaG 126111168580452537982500  ==  [1,2,3,4,5,6]
--
-- Comprobar con QuickCheck que ambas funciones son inversas; es decir,
--    decodificaG (codificaG xs) = xs
--    codificaG (decodificaG n) = n
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Codificacion_de_Godel where

import Data.List (genericLength, group)
import Data.Numbers.Primes (primes, primeFactors)
import Test.QuickCheck (NonNegative (getNonNegative),
                        NonEmptyList (NonEmpty),
                        Positive (Positive, getPositive), quickCheck)

-- 1ª definición de codificaG
-- ==========================

codificaG1 :: [Integer] -> Integer
codificaG1 = codificaG1' . codificaAux

codificaAux :: [Integer] -> [Integer]
codificaAux = map succ

codificaG1' :: [Integer] -> Integer
codificaG1' = aux primes
  where aux _ []          = 1
        aux (p:ps) (x:xs) = p^x * aux ps xs

-- 2ª definición de codificaG
-- ==========================

codificaG2 :: [Integer] -> Integer
codificaG2 = codificaG2' . codificaAux

codificaG2' :: [Integer] -> Integer
codificaG2' xs = product [p^x | (p, x) <- zip primes xs]

-- 3ª definición de codificaG
-- ==========================

codificaG3 :: [Integer] -> Integer
codificaG3 = codificaG3' . codificaAux

codificaG3' :: [Integer] -> Integer
codificaG3' xs = product (zipWith (^) primes xs)

-- 4ª definición de codificaG
-- ==========================

codificaG4 :: [Integer] -> Integer
codificaG4 = codificaG4' . codificaAux

codificaG4' :: [Integer] -> Integer
codificaG4' = product . zipWith (^) primes

-- Comprobación de equivalencia de codificaG
-- =========================================

-- La propiedad es
prop_codificaG :: [NonNegative Integer] -> Bool
prop_codificaG xs =
  all (== codificaG1 ys)
      [codificaG2 ys,
       codificaG3 ys,
       codificaG4 ys]
  where ys = map getNonNegative xs

-- La comprobación es
--    λ> quickCheck prop_codificaG
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de codificaG
-- ======================================

-- La comparación es
--    λ> length (show (codificaG1 (replicate (4*10^4) 1)))
--    208100
--    (1.73 secs, 2,312,100,136 bytes)
--    λ> length (show (codificaG2 (replicate (4*10^4) 1)))
--    208100
--    (1.63 secs, 2,327,676,832 bytes)
--    λ> length (show (codificaG3 (replicate (4*10^4) 1)))
--    208100
--    (1.62 secs, 2,323,836,832 bytes)
--    λ> length (show (codificaG4 (replicate (4*10^4) 1)))
--    208100
--    (1.54 secs, 2,147,635,680 bytes)

-- Definición de codificaG
-- =======================

-- Usaremos la 4ª
codificaG :: [Integer] -> Integer
codificaG = codificaG4

-- Definición de decodificaG
-- =========================

decodificaG :: Integer -> [Integer]
decodificaG = decodificaAux . decodificaG'

decodificaAux :: [Integer] -> [Integer]
decodificaAux = map pred

decodificaG' :: Integer -> [Integer]
decodificaG' 1 = [0]
decodificaG' n = aux primes (group (primeFactors n))
  where aux _ [] = []
        aux (x:xs) ((y:ys):yss) | x == y    = 1 + genericLength ys : aux xs yss
                                | otherwise = 0 : aux xs ((y:ys):yss)

-- Comprobación de propiedades
-- ===========================

-- La primera propiedad es
prop_decodificaG_codificaG :: NonEmptyList (NonNegative Integer) -> Bool
prop_decodificaG_codificaG (NonEmpty xs) = 
  decodificaG (codificaG ys) == ys
  where ys = map getNonNegative xs

-- La comprobación es
--    λ> quickCheck prop_decodificaG_codificaG
--    +++ OK, passed 100 tests.

-- La 2ª propiedad es
prop_codificaG_decodificaG :: Positive Integer -> Bool
prop_codificaG_decodificaG (Positive n) = 
  codificaG (decodificaG n) == n

-- la comprobación es
--    λ> quickCheck prop_codificaG_decodificaG
--    +++ OK, passed 100 tests.


