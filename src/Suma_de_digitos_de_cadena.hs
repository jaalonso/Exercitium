-- Suma_de_digitos_de_cadena.hs
-- Suma de los dígitos de una cadena.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDigitos :: String -> Int
-- tal que (sumaDigitos xs) es la suma de los dígitos de la cadena
-- xs. Por ejemplo,
--    sumaDigitos "SE 2431 X"  ==  10
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Suma_de_digitos_de_cadena where

import Data.Char (digitToInt, isDigit)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaDigitos1 :: String -> Int
sumaDigitos1 xs = sum [digitToInt x | x <- xs, isDigit x]

-- 2ª solución
-- ===========

sumaDigitos2 :: String -> Int
sumaDigitos2 [] = 0
sumaDigitos2 (x:xs)
  | isDigit x  = digitToInt x + sumaDigitos2 xs
  | otherwise  = sumaDigitos2 xs

-- 3ª solución
-- ===========

sumaDigitos3 :: String -> Int
sumaDigitos3 xs = sum (map digitToInt (filter isDigit xs))

-- 4ª solución
-- ===========

sumaDigitos4 :: String -> Int
sumaDigitos4 = sum . map digitToInt . filter isDigit

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaDigitos :: String -> Bool
prop_sumaDigitos xs =
  all (== sumaDigitos1 xs)
      [sumaDigitos2 xs,
       sumaDigitos3 xs,
       sumaDigitos4 xs]

-- La comprobación es
--    λ> quickCheck prop_sumaDigitos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaDigitos1 (take (4*10^6) (cycle "ab12"))
--    3000000
--    (1.92 secs, 819,045,328 bytes)
--    λ> sumaDigitos2 (take (4*10^6) (cycle "ab12"))
--    3000000
--    (1.79 secs, 856,419,112 bytes)
--    λ> sumaDigitos3 (take (4*10^6) (cycle "ab12"))
--    3000000
--    (0.62 secs, 723,045,296 bytes)
--    λ> sumaDigitos4 (take (4*10^6) (cycle "ab12"))
--    3000000
--    (0.63 secs, 723,045,552 bytes)
