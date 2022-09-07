-- Suma_de_los_primeros_numeros_naturales.hs
-- Suma de los primeros números naturales
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    suma :: Integer -> Integer
-- tal (suma n) es la suma de los n primeros números. Por ejemplo,
--    suma 3  ==  6
--    length (show (suma (10^100)))  ==  200
-- ---------------------------------------------------------------------

module Suma_de_los_primeros_numeros_naturales where

import Data.List (foldl')
import Test.QuickCheck

-- 1ª solución
-- ===========

suma1 :: Integer -> Integer
suma1 n = sum [1..n]

-- 2ª solución
-- ===========

suma2 :: Integer -> Integer
suma2 n = (1+n)*n `div` 2

-- 3ª solución
-- ===========

suma3 :: Integer -> Integer
suma3 1 = 1
suma3 n = n + suma3 (n-1)

-- 4ª solución
-- ===========

suma4 :: Integer -> Integer
suma4 n = foldl (+) 0 [0..n]

-- 5ª solución
-- ===========

suma5 :: Integer -> Integer
suma5 n = foldl' (+) 0 [0..n]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_suma :: Positive Integer -> Bool
prop_suma (Positive n) =
  all (== suma1 n)
      [suma2 n,
       suma3 n,
       suma4 n,
       suma5 n]

-- La comprobación es
--    λ> quickCheck prop_suma
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> suma1 (5*10^6)
--    12500002500000
--    (1.23 secs, 806,692,792 bytes)
--    λ> suma2 (5*10^6)
--    12500002500000
--    (0.02 secs, 559,064 bytes)
--    λ> suma3 (5*10^6)
--    12500002500000
--    (3.06 secs, 1,214,684,352 bytes)
--    λ> suma4 (5*10^6)
--    12500002500000
--    (1.25 secs, 806,692,848 bytes)
--    λ> suma5 (5*10^6)
--    12500002500000
--    (0.26 secs, 440,559,048 bytes)
