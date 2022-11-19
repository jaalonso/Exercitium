-- Maximo_de_una_lista.hs
-- Máximo de una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    maximo :: Ord a => [a] -> a
-- tal que (maximo xs) es el máximo de la lista xs. Por ejemplo,
--    maximo [3,7,2,5]                  ==  7
--    maximo ["todo","es","falso"]      ==  "todo"
--    maximo ["menos","alguna","cosa"]  ==  "menos"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Maximo_de_una_lista where

import Data.List (foldl1')
import Test.QuickCheck

-- 1ª solución
-- ===========

maximo1 :: Ord a => [a] -> a
maximo1 [x]      = x
maximo1 (x:y:ys) = max x (maximo1 (y:ys))

-- 2ª solución
-- ===========

maximo2 :: Ord a => [a] -> a
maximo2 = foldr1 max

-- 3ª solución
-- ===========

maximo3 :: Ord a => [a] -> a
maximo3 = foldl1' max

-- 4ª solución
-- ===========

maximo4 :: Ord a => [a] -> a
maximo4 = maximum

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_maximo :: NonEmptyList Int -> Bool
prop_maximo (NonEmpty xs) =
  all (== maximo1 xs)
      [maximo2 xs,
       maximo3 xs]

-- La comprobación es
--    λ> quickCheck prop_maximo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximo1 [0..5*10^6]
--    5000000
--    (3.42 secs, 1,783,406,728 bytes)
--    λ> maximo2 [0..5*10^6]
--    5000000
--    (0.80 secs, 934,638,080 bytes)
--    λ> maximo3 [0..5*10^6]
--    5000000
--    (0.12 secs, 360,591,360 bytes)
--    λ> maximo4 [0..5*10^6]
--    5000000
--    (1.40 secs, 892,891,608 bytes)
