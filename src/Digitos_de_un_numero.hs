-- Digitos_de_un_numero.hs
-- Dígitos de un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    digitos :: Integer -> [Int]
-- tal que (digitos n) es la lista de los dígitos del número n. Por
-- ejemplo,
--    digitos 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

module Digitos_de_un_numero where

import Data.Char (digitToInt)
import qualified Data.Digits as D (digits)
import qualified Data.FastDigits as FD (digits)
import Test.QuickCheck

-- 1ª solución
-- ===========

digitos1 :: Integer -> [Int]
digitos1 n = map fromInteger (aux n)
  where aux :: Integer -> [Integer]
        aux m
          | m < 10    = [m]
          | otherwise = aux (m `div` 10) ++ [m `rem` 10]

-- 2ª solución
-- ===========

digitos2 :: Integer -> [Int]
digitos2 n = map fromInteger (reverse (aux n))
  where aux :: Integer -> [Integer]
        aux m
          | m < 10    = [m]
          | otherwise = (m `rem` 10) : aux (m `div` 10)

-- 3ª solución
-- ===========

digitos3 :: Integer -> [Int]
digitos3 n = map fromInteger (aux [] n)
  where aux :: [Integer] -> Integer -> [Integer]
        aux ds m
          | m < 10    = m : ds
          | otherwise = aux (m `rem` 10 : ds) (m `div` 10)

-- 4ª solución
-- ===========

digitos4 :: Integer -> [Int]
digitos4 n = [read [x] | x <- show n]

-- 5ª solución
-- ===========

digitos5 :: Integer -> [Int]
digitos5 n = map (\ x -> read [x]) (show n)

-- 6ª solución
-- ===========

digitos6 :: Integer -> [Int]
digitos6 = map (read . return) . show

-- 7ª solución
-- ===========

digitos7 :: Integer -> [Int]
digitos7 n = map digitToInt (show n)

-- 8ª solución
-- ===========

digitos8 :: Integer -> [Int]
digitos8 = map digitToInt . show

-- 9ª solución
-- ===========

digitos9 :: Integer -> [Int]
digitos9 0 = [0]
digitos9 n = map fromInteger (D.digits 10 n)

-- 10ª solución
-- ===========

digitos10 :: Integer -> [Int]
digitos10 0 = [0]
digitos10 n = reverse (FD.digits 10 n)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_digitos :: NonNegative Integer -> Bool
prop_digitos (NonNegative n) =
  all (== digitos1 n)
      [digitos2 n,
       digitos3 n,
       digitos4 n,
       digitos5 n,
       digitos6 n,
       digitos7 n,
       digitos8 n,
       digitos9 n,
       digitos10 n]

-- La comprobación es
--    λ> quickCheck prop_digitos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> n = product [1..5000]
--    λ> length (digitos1 n)
--    16326
--    (3.00 secs, 11,701,450,912 bytes)
--    λ> length (digitos2 n)
--    16326
--    (0.13 secs, 83,393,816 bytes)
--    λ> length (digitos3 n)
--    16326
--    (0.11 secs, 83,132,552 bytes)
--    λ> length (digitos4 n)
--    16326
--    (0.01 secs, 23,054,920 bytes)
--    λ> length (digitos5 n)
--    16326
--    (0.01 secs, 22,663,088 bytes)
--    λ> length (digitos6 n)
--    16326
--    (0.06 secs, 22,663,224 bytes)
--    λ> length (digitos7 n)
--    16326
--    (0.01 secs, 22,663,064 bytes)
--    λ> length (digitos8 n)
--    16326
--    (0.03 secs, 22,663,192 bytes)
--    λ> length (digitos9 n)
--    16326
--    (0.05 secs, 82,609,944 bytes)
--    λ> length (digitos10 n)
--    16326
--    (0.01 secs, 26,295,416 bytes)
--
--    λ> n = product [1..5*10^4]
--    λ> length (digitos2 n)
--    213237
--    (10.17 secs, 12,143,633,056 bytes)
--    λ> length (digitos3 n)
--    213237
--    (10.54 secs, 12,140,221,216 bytes)
--    λ> length (digitos4 n)
--    213237
--    (1.29 secs, 2,638,199,328 bytes)
--    λ> length (digitos5 n)
--    213237
--    (2.48 secs, 2,633,081,632 bytes)
--    λ> length (digitos6 n)
--    213237
--    (2.59 secs, 2,633,081,600 bytes)
--    λ> length (digitos7 n)
--    213237
--    (2.55 secs, 2,633,081,608 bytes)
--    λ> length (digitos8 n)
--    213237
--    (2.49 secs, 2,633,081,600 bytes)
--    λ> length (digitos9 n)
--    213237
--    (7.07 secs, 12,133,397,456 bytes)
--    λ> length (digitos10 n)
--    213237
--    (2.47 secs, 2,725,182,064 bytes)
