-- Suma_de_los_digitos_de_un_numero.hs
-- Suma de los digitos de un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDigitos :: Integer -> Integer
-- tal que (sumaDigitos n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitos 3     ==  3
--    sumaDigitos 2454  == 15
--    sumaDigitos 20045 == 11
-- ---------------------------------------------------------------------

module Suma_de_los_digitos_de_un_numero where

import Data.List (foldl')
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
sumaDigitos2 n = foldl' (+) 0 (digitos n)

-- 3ª solución
-- ===========

sumaDigitos3 :: Integer -> Integer
sumaDigitos3 n
  | n < 10    = n
  | otherwise = n `rem` 10 + sumaDigitos3 (n `div` 10)

-- 4ª solución
-- ===========

sumaDigitos4 :: Integer -> Integer
sumaDigitos4 = aux 0
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
       sumaDigitos3 n,
       sumaDigitos4 n]

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
--    (0.41 secs, 660,579,064 bytes)
--    λ> sumaDigitos3 (product [1..2*10^4])
--    325494
--    (1.58 secs, 1,647,082,224 bytes)
--    λ> sumaDigitos4 (product [1..2*10^4])
--    325494
--    (1.72 secs, 1,662,177,792 bytes)
--
--    λ> sumaDigitos1 (product [1..5*10^4])
--    903555
--    (2.51 secs, 3,411,722,136 bytes)
--    λ> sumaDigitos2 (product [1..5*10^4])
--    903555
--    (2.30 secs, 3,396,802,856 bytes)
