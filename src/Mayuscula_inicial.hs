-- Mayuscula_inicial.hs
-- Poner en mayúscula la primera letra y las restantes en minúsculas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    mayusculaInicial :: String -> String
-- tal que (mayusculaInicial xs) es la palabra xs con la letra inicial
-- en mayúscula y las restantes en minúsculas. Por ejemplo,
--    mayusculaInicial "sEviLLa"  ==  "Sevilla"
--    mayusculaInicial ""         ==  ""
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mayuscula_inicial where

import Data.Char (toUpper, toLower)
import Test.QuickCheck

-- 1ª solución
-- ===========

mayusculaInicial1 :: String -> String
mayusculaInicial1 []     = []
mayusculaInicial1 (x:xs) = toUpper x : [toLower y | y <- xs]

-- 2ª solución
-- ===========

mayusculaInicial2 :: String -> String
mayusculaInicial2 [] = []
mayusculaInicial2 (x:xs) = toUpper x : aux xs
  where aux (y:ys) = toLower y : aux ys
        aux []     = []

-- 3ª solución
-- ===========

mayusculaInicial3 :: String -> String
mayusculaInicial3 [] = []
mayusculaInicial3 (x:xs) = toUpper x : map toLower xs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_mayusculaInicial :: String -> Bool
prop_mayusculaInicial xs =
  all (== mayusculaInicial1 xs)
      [mayusculaInicial2 xs,
       mayusculaInicial3 xs]

-- La comprobación es
--    λ> quickCheck prop_mayusculaInicial
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (mayusculaInicial1 (take (10^7) (cycle "aA")))
--    10000000
--    (2.22 secs, 1,680,592,240 bytes)
--    λ> length (mayusculaInicial2 (take (10^7) (cycle "aA")))
--    10000000
--    (2.57 secs, 2,240,592,192 bytes)
--    λ> length (mayusculaInicial3 (take (10^7) (cycle "aA")))
--    10000000
--    (0.16 secs, 1,440,592,192 bytes)
