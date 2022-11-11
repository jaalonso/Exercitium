-- Posiciones_de_un_caracter_en_una_cadena.hs
-- Posiciones de un carácter en una cadena.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    posiciones :: Char -> String -> [Int]
-- tal que (posiciones x ys) es la lista de la posiciones del carácter x
-- en la cadena ys. Por ejemplo,
--    posiciones 'a' "Salamamca"   ==  [1,3,5,8]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Posiciones_de_un_caracter_en_una_cadena where

import Data.List (elemIndices)
import Test.QuickCheck

-- 1ª solución
-- ===========

posiciones1 :: Char -> String -> [Int]
posiciones1 x ys = [n | (y,n) <- zip ys [0..], y == x]

-- 2ª solución
-- ===========

posiciones2 :: Char -> String -> [Int]
posiciones2 x ys = aux x ys 0
  where
    aux _ [] _ = []
    aux b (a:as) n | a == b    = n : aux b as (n+1)
                   | otherwise = aux b as (n+1)

-- 3ª solución
-- ===========

posiciones3 :: Char -> String -> [Int]
posiciones3 = elemIndices

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_posiciones :: Char -> String -> Bool
prop_posiciones x ys =
  all (== posiciones1 x ys)
      [posiciones2 x ys,
       posiciones3 x ys]

-- La comprobación es
--    λ> quickCheck prop_posiciones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (posiciones1 'a' (take (6*10^6) (cycle "abc")))
--    2000000
--    (2.48 secs, 1,680,591,672 bytes)
--    λ> length (posiciones2 'a' (take (6*10^6) (cycle "abc")))
--    2000000
--    (2.98 secs, 1,584,591,720 bytes)
--    λ> length (posiciones3 'a' (take (6*10^6) (cycle "abc")))
--    2000000
--    (0.11 secs, 496,591,600 bytes)
