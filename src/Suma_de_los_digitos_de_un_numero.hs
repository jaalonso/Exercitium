-- Suma_de_los_digitos_de_un_numero.hs
-- Suma de los digitos de un número
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

module Suma_de_los_digitos_de_un_numero where

import Test.QuickCheck

-- 1ª solución
-- ===========

sumaDigitos1 :: Integer -> Integer
sumaDigitos1 n = sum (digitos n)

-- (digitos n) es la lista de los dígitos del número n. Por ejemplo,
--    digitos 320274  ==  [3,2,0,2,7,4]
digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- Nota. En lugar de la definición anterior de digitos se puede usar
-- cualquiera del ejercicio "Dígitos de un número" https://bit.ly/3Tkhc2T

-- 2ª solución
-- ===========

sumaDigitos2 :: Integer -> Integer
sumaDigitos2 n
  | n < 10    = n
  | otherwise = n `rem` 10 + sumaDigitos2 (n `div` 10)

-- 3ª solución
-- ===========

sumaDigitos3 :: Integer -> Integer
sumaDigitos3 = aux 0
  where aux r n
          | n < 10    = r + n
          | otherwise = aux (r + n `rem` 10) (n `div` 10)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaDigitos :: NonNegative Integer -> Bool
prop_sumaDigitos (NonNegative n) =
  all (== sumaDigitos1 n)
      [sumaDigitos2 n,
       sumaDigitos3 n]

-- La comprobación es
--    λ> quickCheck prop_sumaDigitos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaDigitos1 (product [1..2*10^4])
--    325494
--    (0.64 secs, 665,965,832 bytes)
--    λ> sumaDigitos2 (product [1..2*10^4])
--    325494
--    (1.76 secs, 1,647,082,224 bytes)
--    λ> sumaDigitos3 (product [1..2*10^4])
--    325494
--    (1.72 secs, 1,662,177,792 bytes)
