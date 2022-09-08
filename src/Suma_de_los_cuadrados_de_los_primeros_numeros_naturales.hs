-- Suma_de_los_cuadrados_de_los_primeros_numeros_naturales.hs
-- Suma de los cuadrados de los primeros números naturales
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDeCuadrados :: Integer -> Integer
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n números; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
--    length (show (sumaDeCuadrados (10^100)))  ==  300
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Suma_de_los_cuadrados_de_los_primeros_numeros_naturales where

import Data.List (foldl')
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaDeCuadrados1 :: Integer -> Integer
sumaDeCuadrados1 n = sum [x^2 | x <- [1..n]]

-- 2ª solución
-- ===========

sumaDeCuadrados2 :: Integer -> Integer
sumaDeCuadrados2 n = n*(n+1)*(2*n+1) `div` 6

-- 3ª solución
-- ===========

sumaDeCuadrados3 :: Integer -> Integer
sumaDeCuadrados3 1 = 1
sumaDeCuadrados3 n = n^2 + sumaDeCuadrados3 (n-1)

-- 4ª solución
-- ===========

sumaDeCuadrados4 :: Integer -> Integer
sumaDeCuadrados4 n = foldl (+) 0 (map (^2) [0..n])

-- 5ª solución
-- ===========

sumaDeCuadrados5 :: Integer -> Integer
sumaDeCuadrados5 n = foldl' (+) 0 (map (^2) [0..n])

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaDeCuadrados :: Positive Integer -> Bool
prop_sumaDeCuadrados (Positive n) =
  all (== sumaDeCuadrados1 n)
      [sumaDeCuadrados2 n,
       sumaDeCuadrados3 n,
       sumaDeCuadrados4 n,
       sumaDeCuadrados5 n]

-- La comprobación es
--    λ> quickCheck prop_sumaDeCuadrados
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaDeCuadrados1 (2*10^6)
--    2666668666667000000
--    (1.90 secs, 1,395,835,576 bytes)
--    λ> sumaDeCuadrados2 (2*10^6)
--    2666668666667000000
--    (0.01 secs, 563,168 bytes)
--    λ> sumaDeCuadrados3 (2*10^6)
--    2666668666667000000
--    (2.37 secs, 1,414,199,400 bytes)
--    λ> sumaDeCuadrados4 (2*10^6)
--    2666668666667000000
--    (1.33 secs, 1,315,836,128 bytes)
--    λ> sumaDeCuadrados5 (2*10^6)
--    2666668666667000000
--    (0.71 secs, 1,168,563,384 bytes)
