-- Diccionario_de_frecuencias.hs
-- Diccionario de frecuencias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    frecuencias :: Ord a => [a] -> Map a Int
-- tal que (frecuencias xs) es el diccionario formado por los elementos
-- de xs junto con el número de veces que aparecen en xs. Por ejemplo, 
--    λ> frecuencias "sosos"
--    fromList [('o',2),('s',3)]
--    λ> frecuencias (show (10^100))
--    fromList [('0',100),('1',1)]
--    λ> frecuencias (take (10^6) (cycle "abc"))
--    fromList [('a',333334),('b',333333),('c',333333)]
--    λ> size (frecuencias (take (10^6) (cycle [1..10^6])))
--    1000000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Diccionario_de_frecuencias where

import Data.List (foldl')
import Data.Map (Map, empty, insertWith, fromList, fromListWith, size)
import Test.QuickCheck

-- 1ª solución
-- ===========

frecuencias1 :: Ord a => [a] -> Map a Int
frecuencias1 []     = empty
frecuencias1 (x:xs) = insertWith (+) x 1 (frecuencias1 xs)

-- 2ª solución
-- ===========

frecuencias2 :: Ord a => [a] -> Map a Int
frecuencias2 = foldl' (\d x-> insertWith (+) x 1 d) empty

-- 3ª solución
-- ===========

frecuencias3 :: Ord a => [a] -> Map a Int
frecuencias3 xs = fromListWith (+) (zip xs (repeat 1))

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_frecuencias :: [Int] -> Bool
prop_frecuencias xs =
  all (== frecuencias1 xs)
      [ frecuencias2 xs
      , frecuencias3 xs]

-- La comprobación es
--    λ> quickCheck prop_frecuencias
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> frecuencias1 (take (10^6) (cycle "abc"))
--    fromList [('a',333334),('b',333333),('c',333333)]
--    (0.89 secs, 453,842,448 bytes)
--    λ> frecuencias2 (take (10^6) (cycle "abc"))
--    fromList [('a',333334),('b',333333),('c',333333)]
--    (0.54 secs, 274,181,128 bytes)
--    λ> frecuencias3 (take (10^6) (cycle "abc"))
--    fromList [('a',333334),('b',333333),('c',333333)]
--    (0.29 secs, 313,787,976 bytes)
     
--    λ> size (frecuencias1 (take (10^6) (cycle [1..10^6])))
--    1000000
--    (3.76 secs, 2,651,926,024 bytes)
--    λ> size (frecuencias2 (take (10^6) (cycle [1..10^6])))
--    1000000
--    (1.03 secs, 1,640,678,448 bytes)
--    λ> size (frecuencias3 (take (10^6) (cycle [1..10^6])))
--    1000000
--    (0.88 secs, 1,672,678,536 bytes)
