-- Segmentos_cuyos_elementos_cumple_una_propiedad.hs
-- Segmentos cuyos elementos cumplen una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [[a]]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Segmentos_cuyos_elementos_cumple_una_propiedad where

import Data.List.Split (splitWhen)
import Test.QuickCheck.HigherOrder (quickCheck')

-- 1ª solución
-- ===========

segmentos1 :: (a -> Bool) -> [a] -> [[a]]
segmentos1 _ [] = []
segmentos1 p (x:xs)
  | p x       = takeWhile p (x:xs) : segmentos1 p (dropWhile p xs)
  | otherwise = segmentos1 p xs

-- 2ª solución
-- ===========

segmentos2 :: (a -> Bool) -> [a] -> [[a]]
segmentos2 p xs = filter (not .null) (splitWhen (not . p) xs)

-- 3ª solución
-- ===========

segmentos3 :: (a -> Bool) -> [a] -> [[a]]
segmentos3 = (filter (not . null) .) . splitWhen . (not .)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_segmentos :: (Int -> Bool) -> [Int] -> Bool
prop_segmentos p xs =
  all (== segmentos1 p xs)
      [segmentos2 p xs,
       segmentos3 p xs]

-- La comprobación es
--    λ> quickCheck' prop_segmentos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (segmentos1 even [1..5*10^6])
--    2500000
--    (2.52 secs, 2,080,591,088 bytes)
--    λ> length (segmentos2 even [1..5*10^6])
--    2500000
--    (0.78 secs, 2,860,591,688 bytes)
--    λ> length (segmentos3 even [1..5*10^6])
--    2500000
--    (0.82 secs, 2,860,592,000 bytes)
