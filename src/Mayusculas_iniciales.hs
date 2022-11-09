-- Mayusculas_iniciales.hs
-- Mayúsculas iniciales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran las siguientes reglas de mayúsculas iniciales para los
-- títulos:
-- + la primera palabra comienza en mayúscula y
-- + todas las palabras que tienen 4 letras como mínimo empiezan con
--   mayúsculas
--
-- Definir la función
--    titulo :: [String] -> [String]
-- tal que (titulo ps) es la lista de las palabras de ps con
-- las reglas de mayúsculas iniciales de los títulos. Por ejemplo,
--    λ> titulo ["eL","arTE","DE","La","proGraMacion"]
--    ["El","Arte","de","la","Programacion"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mayusculas_iniciales where

import Data.Char (toUpper, toLower)
import Test.QuickCheck

-- 1ª solución
-- ===========

titulo1 :: [String] -> [String]
titulo1 []     = []
titulo1 (p:ps) = mayusculaInicial p : [transforma q | q <- ps]

-- (mayusculaInicial xs) es la palabra xs con la letra inicial
-- en mayúscula y las restantes en minúsculas. Por ejemplo,
--    mayusculaInicial "sEviLLa"  ==  "Sevilla"
mayusculaInicial :: String -> String
mayusculaInicial []     = []
mayusculaInicial (x:xs) = toUpper x : [toLower y | y <- xs]

-- (transforma p) es la palabra p con mayúscula inicial si su longitud
-- es mayor o igual que 4 y es p en minúscula en caso contrario
transforma :: String -> String
transforma p | length p >= 4 = mayusculaInicial p
             | otherwise     = minuscula p

-- (minuscula xs) es la palabra xs en minúscula.
minuscula :: String -> String
minuscula xs = [toLower x | x <- xs]

-- 2ª solución
-- ===========

titulo2 :: [String] -> [String]
titulo2 []     = []
titulo2 (p:ps) = mayusculaInicial p : aux ps
  where aux []     = []
        aux (q:qs) = transforma q : aux qs

-- 3ª solución
-- ===========

titulo3 :: [String] -> [String]
titulo3 []     = []
titulo3 (p:ps) = mayusculaInicial p : map transforma ps

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_titulo :: [String] -> Bool
prop_titulo xs =
  all (== titulo1 xs)
      [titulo2 xs,
       titulo3 xs]

-- La comprobación es
--    λ> quickCheck prop_titulo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (titulo1 (take (10^7) (cycle ["hOy","Es","juEves","dE","Noviembre"])))
--    10000000
--    (2.17 secs, 1,680,592,512 bytes)
--    λ> length (titulo2 (take (10^7) (cycle ["hOy","Es","juEves","dE","Noviembre"])))
--    10000000
--    (2.45 secs, 2,240,592,464 bytes)
--    λ> length (titulo3 (take (10^7) (cycle ["hOy","Es","juEves","dE","Noviembre"])))
--    10000000
--    (0.16 secs, 1,440,592,464 bytes)
