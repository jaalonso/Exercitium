-- Rompecabeza_matematico.hs
-- Rompecabeza matemático.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-Noviembre-2014 (actualizado 30-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir una función
--    f :: Int -> Int
-- tal que para todo n, f(f(n)) = -n y comprobar con QuickCheck que se
-- cumple la propiedad
--    prop_f :: Int -> Bool
--    prop_f n = f (f n) == -n
-- es decir,
--    λ> quickCheck prop_f
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Rompecabeza_matematico where

import Test.QuickCheck

-- 1ª solución
-- ===========

f1 :: Int -> Int
f1 n | even n && n > 0 = n-1
     | even n && n < 0 = n+1
     | odd  n && n > 0 = -n-1
     | odd  n && n < 0 = -n+1
     | otherwise       = 0

-- La propiedad es
prop_f1 :: Int -> Bool
prop_f1 n = f1 (f1 n) == -n

-- La comprobación es
--    λ> quickCheck prop_f1
--    +++ OK, passed 100 tests.

-- 2ª solución
-- ===========

f2 :: Int -> Int
f2 n | even n    =  n - signum n
     | otherwise = -n - signum n

-- La propiedad es
prop_f2 :: Int -> Bool
prop_f2 n = f2 (f2 n) == -n

-- La comprobación es
--    λ> quickCheck prop_f2
--    +++ OK, passed 100 tests.

-- 3ª solución
-- ===========

f3 :: Int -> Int
f3 n = n * (2 * mod n 2 - 1) + signum n

-- La propiedad es
prop_f3 :: Int -> Bool
prop_f3 n = f3 (f3 n) == -n

-- La comprobación es
--    λ> quickCheck prop_f3
--    +++ OK, passed 100 tests.

-- 4ª solución
-- ===========

f4 :: Int -> Int
f4 n = (-1)^(abs n)*n - signum n

-- La propiedad es
prop_f4 :: Int -> Bool
prop_f4 n = f4 (f4 n) == -n

-- La comprobación es
--    λ> quickCheck prop_f4
--    +++ OK, passed 100 tests.

-- Verificación
-- ============

-- La propiedad es
prop_f :: Int -> Bool
prop_f n =
  and [f (f n) == -n | f <- [f1, f2, f3, f4]]

-- La comprobación es
--    λ> quickCheck prop_f
--    +++ OK, passed 100 tests.
