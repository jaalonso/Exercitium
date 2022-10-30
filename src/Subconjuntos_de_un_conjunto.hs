-- Subconjuntos_de_un_conjunto.hs
-- Subconjuntos_de_un_conjunto
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos xs) es la lista de las subconjuntos de la lista
-- xs. Por ejemplo,
--    λ> subconjuntos [2,3,4]
--    [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
--    λ> subconjuntos [1,2,3,4]
--    [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],
--       [2,3,4],  [2,3],  [2,4],  [2],  [3,4],  [3],  [4], []]
--
-- Comprobar con QuickChek que el número de elementos de
-- (subconjuntos xs) es 2 elevado al número de elementos de xs.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_length_subconjuntos
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Subconjuntos_de_un_conjunto where

import Data.List (sort, subsequences)
import Test.QuickCheck

-- 1ª solución
-- ===========

subconjuntos1 :: [a] -> [[a]]
subconjuntos1 []     = [[]]
subconjuntos1 (x:xs) = [x:ys | ys <- sub] ++ sub
  where sub = subconjuntos1 xs

-- 2ª solución
-- ===========

subconjuntos2 :: [a] -> [[a]]
subconjuntos2 []     = [[]]
subconjuntos2 (x:xs) = map (x:) sub ++ sub
  where sub = subconjuntos2 xs

-- 3ª solución
-- ===========

subconjuntos3 :: [a] -> [[a]]
subconjuntos3 = subsequences

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_subconjuntos :: [Int] -> Bool
prop_subconjuntos xs =
  all (== sort (subconjuntos1 xs))
      [sort (subconjuntos2 xs),
       sort (subconjuntos3 xs)]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_subconjuntos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (subconjuntos1 [1..23])
--    8388608
--    (2.05 secs, 1,476,991,840 bytes)
--    λ> length (subconjuntos2 [1..23])
--    8388608
--    (0.87 secs, 1,208,555,312 bytes)
--    λ> length (subconjuntos3 [1..23])
--    8388608
--    (0.09 secs, 873,006,608 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_length_subconjuntos :: [Int] -> Bool
prop_length_subconjuntos xs =
  length (subconjuntos1 xs) == 2 ^ length xs

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_length_subconjuntos
--    +++ OK, passed 100 tests.
