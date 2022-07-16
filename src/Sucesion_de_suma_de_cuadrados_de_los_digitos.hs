-- Sucesion_de_suma_de_cuadrados_de_los_digitos.hs
-- Sucesión de suma de cuadrados de los dígitos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sucSumaCuadradosDigitos :: Integer -> [Integer]
-- tal que (sucSumaCuadradosDigitos n) es la sucesión cuyo primer
-- término es n y los restantes se obtienen sumando los cuadrados de los
-- dígitos de su término anterior. Por ejemplo, 
--    λ> take 20 (sucSumaCuadradosDigitos1 2000)
--    [2000,4,16,37,58,89,145,42,20,4,16,37,58,89,145,42,20,4,16,37]
--    λ> take 20 (sucSumaCuadradosDigitos 1976)
--    [1976,167,86,100,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
--    λ> sucSumaCuadradosDigitos 2000 !! (10^9)
--    20
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sucesion_de_suma_de_cuadrados_de_los_digitos where

import Test.QuickCheck (Positive (Positive), NonNegative (NonNegative), quickCheck)

-- 1ª solución 
-- ===========

sucSumaCuadradosDigitos1 :: Integer -> [Integer]
sucSumaCuadradosDigitos1 n =
  n : [sumaCuadradosDigitos x | x <- sucSumaCuadradosDigitos1 n]

-- (sumaCuadradosDigitos n) es la suma de los cuadrados de los dígitos
-- de n. Por ejemplo, 
--    sumaCuadradosDigitos 2016  ==  41
sumaCuadradosDigitos :: Integer -> Integer
sumaCuadradosDigitos n = sum (map (^2) (digitos n))

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos n = [read [d] | d <- show n]

-- 2ª solución 
-- ===========

sucSumaCuadradosDigitos2 :: Integer -> [Integer]
sucSumaCuadradosDigitos2 = iterate sumaCuadradosDigitos

-- 3ª solución
-- ===========

-- A partir de los cálculos con las definiciones anteriores, se observa
-- que para todo n (sucSumaCuadradosDigitos n) tiene una parte pura y
-- otra periódica. Por ejemplo, para n = 2016, 
--    λ> take 20 (sucSumaCuadradosDigitos 2016)
--    [2016,41,17,50,25,29,85,89,145,42,20,4,16,37,58,89,145,42,20,4]
-- la parte pura es
--    [2016,41,17,50,25,29,85]
-- y la parte periódica es
--    [89,145,42,20,4,16,37,58])

sucSumaCuadradosDigitos3 :: Integer -> [Integer]
sucSumaCuadradosDigitos3 n = xs ++ cycle ys
  where (xs,ys) = sucCompactaSumaCuadradosDigitos n

-- (sucCompactaSumaCuadradosDigitos n) es el par formado por la parte
-- pura y la periódica de (sucSumaCuadradosDigitos n). Por ejemplo, 
--    λ> sucCompactaSumaCuadradosDigitos 2016
--    ([2016,41,17,50,25,29,85],[89,145,42,20,4,16,37,58])
--    λ> sucCompactaSumaCuadradosDigitos 1976
--    ([1976,167,86,100],[1])
sucCompactaSumaCuadradosDigitos :: Integer -> ([Integer],[Integer])
sucCompactaSumaCuadradosDigitos = 
  partePuraPeriodica . sucSumaCuadradosDigitos1

-- (partePuraPeriodica xs) es el par formado por la parte pura y la
-- periódica de xs. Por ejemplo,
--    λ> partePuraPeriodica (sucSumaCuadradosDigitos 2016)
--    ([2016,41,17,50,25,29,85],[89,145,42,20,4,16,37,58])
--    λ> partePuraPeriodica (sucSumaCuadradosDigitos 1976)
--    ([1976,167,86,100],[1])
partePuraPeriodica :: [Integer] -> ([Integer],[Integer])
partePuraPeriodica = aux [] 
  where aux as (b:bs) | b `elem` as = span (/=b) (reverse as)
                      | otherwise = aux (b:as) bs

-- 4ª solución
-- ===========

sucSumaCuadradosDigitos4 :: Integer -> [Integer]
sucSumaCuadradosDigitos4 1  = repeat 1
sucSumaCuadradosDigitos4 89 = cycle [89,145,42,20,4,16,37,58]
sucSumaCuadradosDigitos4 n  =
  n : sucSumaCuadradosDigitos4 (sumaCuadradosDigitos n)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sucSumaCuadradosDigitos :: Positive Integer -> NonNegative Int -> Bool
prop_sucSumaCuadradosDigitos (Positive n) (NonNegative k) =
  all (== sucSumaCuadradosDigitos1 n !! k)
      [sucSumaCuadradosDigitos2 n !! k,
       sucSumaCuadradosDigitos3 n !! k,
       sucSumaCuadradosDigitos4 n !! k]

-- La comprobación es
--    λ> quickCheck prop_sucSumaCuadradosDigitos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sucSumaCuadradosDigitos1 2000 !! (10^4)
--    20
--    (6.96 secs, 8,049,886,312 bytes)
--    λ> sucSumaCuadradosDigitos2 2000 !! (10^4)
--    20
--    (0.08 secs, 91,024,688 bytes)
--    λ> sucSumaCuadradosDigitos3 2000 !! (10^4)
--    20
--    (0.01 secs, 995,560 bytes)
--    λ> sucSumaCuadradosDigitos4 2000 !! (10^4)
--    20
--    (0.02 secs, 587,040 bytes)
--    
--    λ> sucSumaCuadradosDigitos2 2000 !! (3*10^5)
--    20
--    (1.96 secs, 2,715,501,416 bytes)
--    λ> sucSumaCuadradosDigitos3 2000 !! (3*10^5)
--    20
--    (0.02 secs, 995,872 bytes)
--    λ> sucSumaCuadradosDigitos4 2000 !! (3*10^5)
--    20
--    (0.02 secs, 587,352 bytes)
--    
--    λ> sucSumaCuadradosDigitos3 2000 !! (10^9)
--    20
--    (2.85 secs, 996,016 bytes)
--    λ> sucSumaCuadradosDigitos4 2000 !! (10^9)
--    20
--    (2.54 secs, 587,496 bytes)

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en "Une application curieuse" en "Quelques apports de
-- l'Informatique à lènseignements des mathematiques" pp. 33-40.

