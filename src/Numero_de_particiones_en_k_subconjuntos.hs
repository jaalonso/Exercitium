-- Numero_de_particiones_en_k_subconjuntos.hs
-- Número de particiones en k subconjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    numeroParticiones :: Int -> Int -> Int
-- tal que (numeroParticiones n k) es el número de particiones de
-- conjunto de n elementos en k subconjuntos disjuntos. Por ejemplo,
--    numeroParticiones 3 2    ==  3
--    numeroParticiones 3 3    ==  1
--    numeroParticiones 4 3    ==  6
--    numeroParticiones 4 1    ==  1
--    numeroParticiones 4 4    ==  1
--    numeroParticiones 91 89  ==  8139495
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numero_de_particiones_en_k_subconjuntos where

import Data.Array (Array, (!), array)
import Test.QuickCheck (Positive (Positive), quickCheckWith)

-- 1ª solución
-- ===========

numeroParticiones1 :: Int -> Int -> Int
numeroParticiones1 n k =
  length (particiones1 [1..n] k)

particiones1 :: [a] -> Int -> [[[a]]]
particiones1 [] _     = []
particiones1 _  0     = []
particiones1 xs 1     = [[xs]]
particiones1 (x:xs) k = [[x]:ys | ys <- particiones1 xs (k-1)] ++ 
                        concat [inserta x ys | ys <- particiones1 xs k]

-- (inserta x yss) es la lista obtenida insertando x en cada uno de los
-- conjuntos de yss. Por ejemplo,
--    inserta 4 [[3],[2,5]]  ==  [[[4,3],[2,5]],[[3],[4,2,5]]]
inserta :: a -> [[a]] -> [[[a]]]
inserta _ []       = []
inserta x (ys:yss) = ((x:ys):yss) : [ys:zss | zss <- inserta x yss]
  
-- 2ª solución
-- ===========

numeroParticiones2 :: Int -> Int -> Int
numeroParticiones2 0 _ = 0
numeroParticiones2 _ 0 = 0
numeroParticiones2 _ 1 = 1
numeroParticiones2 n k = k * numeroParticiones2 (n-1) k +
                         numeroParticiones2 (n-1) (k-1) 

-- 3ª solución
-- ===========

numeroParticiones3 :: Int -> Int -> Int
numeroParticiones3 n k = matrizNumeroParticiones n k ! (n,k)

matrizNumeroParticiones :: Int -> Int -> Array (Int,Int) Int
matrizNumeroParticiones n k = q where
  q = array ((0,0),(n,k)) [((i,j), f i j) | i <- [0..n], j <- [0..k]]
  f _ 0 = 0
  f 0 _ = 0
  f _ 1 = 1
  f i j | i == j    = 1
        | otherwise = j * f (i-1) j + f (i-1) (j-1)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numeroParticiones :: Positive Int -> Positive Int -> Bool
prop_numeroParticiones (Positive n) (Positive k) =
  all (== numeroParticiones1 n k)
      [numeroParticiones2 n k,
       numeroParticiones3 n k]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_numeroParticiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numeroParticiones1 12 6
--    1323652
--    (1.22 secs, 1,152,945,608 bytes)
--    λ> numeroParticiones2 12 6
--    1323652
--    (0.00 secs, 1,283,152 bytes)
--    λ> numeroParticiones3 12 6
--    1323652
--    (0.01 secs, 1,155,864 bytes)
--
--    λ> numeroParticiones2 21 19
--    19285
--    (2.04 secs, 990,274,976 bytes)
--    λ> numeroParticiones3 21 19
--    19285
--    (0.00 secs, 940,736 bytes)
