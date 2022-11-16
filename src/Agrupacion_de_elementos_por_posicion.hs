-- Agrupacion_de_elementos_por_posicion.hs
-- Agrupación de elementos por posición.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por ejemplo,
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--
-- Comprobar con QuickChek que la longitud de todos los elementos de
-- (agrupa xs) es igual a la longitud de xs.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Agrupacion_de_elementos_por_posicion where

import Data.List (transpose)
import qualified Data.Matrix as M (fromLists, toLists, transpose)
import Test.QuickCheck

-- 1ª solución
-- ===========

-- (primeros xss) es la lista de los primeros elementos de xss. Por
-- ejemplo,
--    primeros [[1..6],[7..9],[10..20]]  ==  [1,7,10]
primeros :: [[a]] -> [a]
primeros = map head

-- (restos xss) es la lista de los restos de elementos de xss. Por
-- ejemplo,
--    restos [[1..3],[7,8],[4..7]]  ==  [[2,3],[8],[5,6,7]]
restos :: [[a]] -> [[a]]
restos = map tail

agrupa1 :: Eq a => [[a]] -> [[a]]
agrupa1 []  = []
agrupa1 xss
  | [] `elem` xss = []
  | otherwise     = primeros xss : agrupa1 (restos xss)

-- 2ª solución
-- ===========

-- (conIgualLongitud xss) es la lista obtenida recortando los elementos
-- de xss para que todos tengan la misma longitud. Por ejemplo,
--    > conIgualLongitud [[1..6],[7..9],[10..20]]
--    [[1,2,3],[7,8,9],[10,11,12]]
conIgualLongitud :: [[a]] -> [[a]]
conIgualLongitud xss = map (take n) xss
  where n = minimum (map length xss)

agrupa2 :: Eq a => [[a]] -> [[a]]
agrupa2 = transpose . conIgualLongitud

-- 3ª solución
-- ===========

agrupa3 :: Eq a => [[a]] -> [[a]]
agrupa3 = M.toLists . M.transpose . M.fromLists . conIgualLongitud

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_agrupa :: NonEmptyList [Int] -> Bool
prop_agrupa (NonEmpty xss) =
  all (== agrupa1 xss)
      [agrupa2 xss,
       agrupa3 xss]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (agrupa1 [[1..10^4] | _ <- [1..10^4]])
--    10000
--    (3.96 secs, 16,012,109,904 bytes)
--    λ> length (agrupa2 [[1..10^4] | _ <- [1..10^4]])
--    10000
--    (25.80 secs, 19,906,197,528 bytes)
--    λ> length (agrupa3 [[1..10^4] | _ <- [1..10^4]])
--    10000
--    (9.56 secs, 7,213,797,984 bytes)

-- La comprobación es
--    λ> quickCheck prop_agrupa
--    +++ OK, passed 100 tests.

-- La propiedad es
prop_agrupa_length :: [[Int]] -> Bool
prop_agrupa_length xss =
  and [length xs == n | xs <- agrupa1 xss]
  where n = length xss

-- La comprobación es
--    λ> quickCheck prop_agrupa_length
--    +++ OK, passed 100 tests.
