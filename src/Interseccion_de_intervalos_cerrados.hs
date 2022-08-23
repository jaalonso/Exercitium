-- Interseccion_de_intervalos_cerrados.hs
-- Intersección de intervalos cerrados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los intervalos cerrados se pueden representar mediante una lista de
-- dos números (el primero es el extremo inferior del intervalo y el
-- segundo el superior).
--
-- Definir la función
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2. Por ejemplo,
--    interseccion [] [3,5]     ==  []
--    interseccion [3,5] []     ==  []
--    interseccion [2,4] [6,9]  ==  []
--    interseccion [2,6] [6,9]  ==  [6,6]
--    interseccion [2,6] [0,9]  ==  [2,6]
--    interseccion [2,6] [0,4]  ==  [2,4]
--    interseccion [4,6] [0,4]  ==  [4,4]
--    interseccion [5,6] [0,4]  ==  []
--
-- Comprobar con QuickCheck que la intersección de intervalos es
-- conmutativa.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Interseccion_de_intervalos_cerrados where

import Test.QuickCheck

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1,b1] [a2,b2]
    | a <= b    = [a,b]
    | otherwise = []
    where a = max a1 a2
          b = min b1 b2

-- La propiedad es
prop_interseccion :: Int -> Int -> Int -> Int -> Property
prop_interseccion a1 b1 a2 b2 =
  a1 <= b1 && a2 <= b2 ==>
  interseccion [a1,b1] [a2,b2] == interseccion [a2,b2] [a1,b1]

-- La comprobación es
--    λ> quickCheck prop_interseccion
--    +++ OK, passed 100 tests; 263 discarded.
