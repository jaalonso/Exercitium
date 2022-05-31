-- Suma_de_los_elementos_de_las_diagonales_matrices_espirales.hs
-- Suma de los elementos de las diagonales matrices espirales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Empezando con el número 1 y moviéndose en el sentido de las agujas
-- del reloj se obtienen las matrices espirales 
--    |1 2|   |7 8 9|   | 7  8  9 10|   |21 22 23 24 25|
--    |4 3|   |6 1 2|   | 6  1  2 11|   |20  7  8  9 10|
--            |5 4 3|   | 5  4  3 12|   |19  6  1  2 11|
--                      |16 15 14 13|   |18  5  4  3 12|
--                                      |17 16 15 14 13|
-- La suma los elementos de sus diagonales es
--    + en la 2x2: 1+3+2+4               =  10
--    + en la 3x3: 1+3+5+7+9             =  25
--    + en la 4x4: 1+2+3+4+7+10+13+16    =  56
--    + en la 5x5: 1+3+5+7+9+13+17+21+25 = 101
--
-- Definir la función 
--    sumaDiagonales :: Integer -> Integer
-- tal que (sumaDiagonales n) es la suma de los elementos en las
-- diagonales de la matriz espiral de orden nxn. Por ejemplo.
--    sumaDiagonales 1         ==  1
--    sumaDiagonales 2         ==  10
--    sumaDiagonales 3         ==  25
--    sumaDiagonales 4         ==  56
--    sumaDiagonales 5         ==  101
--    sumaDiagonales (10^6)    ==  666667166668000000
--    sumaDiagonales (1+10^6)  ==  666669166671000001
--
--    sumaDiagonales (10^2)  ==         671800
--    sumaDiagonales (10^3)  ==        667168000
--    sumaDiagonales (10^4)  ==       666716680000
--    sumaDiagonales (10^5)  ==      666671666800000
--    sumaDiagonales (10^6)  ==     666667166668000000
--    sumaDiagonales (10^7)  ==    666666716666680000000
--    sumaDiagonales (10^8)  ==   666666671666666800000000
--    sumaDiagonales (10^9)  ==  666666667166666668000000000
--
-- Comprobar con QuickCheck que el último dígito de (sumaDiagonales n)
-- es 0, 4 ó 6 si n es par y es 1, 5 ó 7 en caso contrario. 
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Suma_de_los_elementos_de_las_diagonales_matrices_espirales where

import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

sumaDiagonales1 :: Integer -> Integer
sumaDiagonales1 = sum . elementosEnDiagonales

-- (elementosEnDiagonales n) es la lista de los elementos en las
-- diagonales de la matriz espiral de orden nxn. Por ejemplo,
--    elementosEnDiagonales 1  ==  [1]
--    elementosEnDiagonales 2  ==  [1,2,3,4]
--    elementosEnDiagonales 3  ==  [1,3,5,7,9]
--    elementosEnDiagonales 4  ==  [1,2,3,4,7,10,13,16]
--    elementosEnDiagonales 5  ==  [1,3,5,7,9,13,17,21,25]
elementosEnDiagonales :: Integer -> [Integer]
elementosEnDiagonales n 
  | even n    = tail (scanl (+) 0 (concatMap (replicate 4) [1,3..n-1]))
  | otherwise = scanl (+) 1 (concatMap (replicate 4) [2,4..n-1])

-- 2ª solución
-- ===========

sumaDiagonales2 :: Integer -> Integer
sumaDiagonales2 n
  | even n    = (-1) + n `div` 2 + sum [2*k^2-k+1 | k <- [0..n]]
  | otherwise = 1 + sum [4*k^2-6*k+6 | k <- [3,5..n]]

-- 3ª solución
-- ===========

sumaDiagonales3 :: Integer -> Integer
sumaDiagonales3 n
  | even n    = n * (4*n^2 + 3*n + 8) `div` 6
  | otherwise = (4*n^3 + 3*n^2 + 8*n - 9) `div` 6

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_sumaDiagonales :: Positive Integer -> Bool
prop_sumaDiagonales (Positive n) =
  all (== sumaDiagonales1 n)
      [sumaDiagonales2 n,
       sumaDiagonales3 n]

-- La comprobación es
--    λ> quickCheck prop_sumaDiagonales_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaDiagonales (2*10^6)
--    5333335333336000000
--    (2.30 secs, 1,521,955,848 bytes)
--    λ> sumaDiagonales2 (2*10^6)
--    5333335333336000000
--    (2.77 secs, 1,971,411,440 bytes)
--    λ> sumaDiagonales3 (2*10^6)
--    5333335333336000000
--    (0.01 secs, 139,520 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_sumaDiagonales2 :: Positive Integer -> Bool
prop_sumaDiagonales2 (Positive n) 
  | even n    = x `elem` [0,4,6] 
  | otherwise = x `elem` [1,5,7] 
  where x = sumaDiagonales1 n `mod` 10

-- La comprobación es
--    λ> quickCheck prop_sumaDiagonales2
--    +++ OK, passed 100 tests.
