-- Concatenacion_de_una_lista_de_listas.hs
-- Concatenación de una lista de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir, por recursión, la función
--    conc :: [[a]] -> [a]
-- tal que (conc xss) es la concenación de las listas de xss. Por
-- ejemplo,
--    conc [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
--
-- Comprobar con QuickCheck que la longitud de (conc xss) es la suma de
-- las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Contenacion_de_una_lista_de_listas where

import Test.QuickCheck

-- 1ª solución
-- ===========

conc1 :: [[a]] -> [a]
conc1 xss = [x | xs <- xss, x <- xs]

-- 2ª solución
-- ===========

conc2 :: [[a]] -> [a]
conc2 []       = []
conc2 (xs:xss) = xs ++ conc2 xss

-- 3ª solución
-- ===========

conc3 :: [[a]] -> [a]
conc3 = foldr (++) []

-- 4ª solución
-- ===========

conc4 :: [[a]] -> [a]
conc4 = concat

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_conc :: [[Int]] -> Bool
prop_conc xss =
  all (== conc1 xss)
      [conc2 xss,
       conc3 xss,
       conc4 xss]

-- La comprobación es
--    λ> quickCheck prop_conc
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (conc1 [[1..n] | n <- [1..5000]])
--    12502500
--    (2.72 secs, 1,802,391,200 bytes)
--    λ> length (conc2 [[1..n] | n <- [1..5000]])
--    12502500
--    (0.27 secs, 1,602,351,160 bytes)
--    λ> length (conc3 [[1..n] | n <- [1..5000]])
--    12502500
--    (0.28 secs, 1,602,071,192 bytes)
--    λ> length (conc4 [[1..n] | n <- [1..5000]])
--    12502500
--    (0.26 secs, 1,602,071,184 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_long_conc :: [[Int]] -> Bool
prop_long_conc xss =
  length (conc1 xss) == sum (map length xss)

-- La comprobación es
--    λ> quickCheck prop_long_conc
--    +++ OK, passed 100 tests.
