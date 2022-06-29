-- Particiones_en_k_subconjuntos.hs
-- Particiones en k subconjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-julio-2022
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Particiones_en_k_subconjuntos where

import Data.List (nub, sort)
import Data.Array (Array, (!), array, listArray)
import Test.QuickCheck (Positive (Positive), quickCheckWith)

-- ---------------------------------------------------------------------
-- Definir la función
--    particiones :: [a] -> Int -> [[[a]]]
-- tal que (particiones xs k) es la lista de las particiones de xs en k
-- subconjuntos disjuntos. Por ejemplo,
--    λ> particiones [2,3,6] 2
--    [[[2],[3,6]],[[2,3],[6]],[[3],[2,6]]]
--    λ> particiones [2,3,6] 3
--    [[[2],[3],[6]]]
--    λ> particiones [4,2,3,6] 3
--    [[[4],[2],[3,6]],[[4],[2,3],[6]],[[4],[3],[2,6]],
--     [[4,2],[3],[6]],[[2],[4,3],[6]],[[2],[3],[4,6]]]
--    λ> particiones [4,2,3,6] 1
--    [[[4,2,3,6]]]
--    λ> particiones [4,2,3,6] 4
--    [[[4],[2],[3],[6]]]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

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

particiones2 :: [a] -> Int -> [[[a]]]
particiones2 [] _     = []
particiones2 _  0     = []
particiones2 xs 1     = [[xs]]
particiones2 (x:xs) k = map ([x]:) (particiones2 xs (k-1)) ++ 
                        concatMap (inserta x) (particiones2 xs k)

-- 3ª solución
-- ===========

particiones3 :: [a] -> Int -> [[[a]]]
particiones3 xs k = matrizParticiones xs k ! (length xs, k)

matrizParticiones :: [a] -> Int -> Array (Int,Int) [[[a]]]
matrizParticiones xs k = q where
  q = array ((0,0),(n,k)) [((i,j), f i j) | i <- [0..n], j <- [0..k]]
  n = length xs
  v = listArray (1,n) xs
  f _ 0 = []
  f 0 _ = []
  f m 1 = [[take m xs]]
  f i j | i == j = [[[x] | x <- take i xs]]
        | otherwise = map ([v!i] :) (q!(i-1,j-1)) ++
                      concatMap (inserta (v!i)) (q!(i-1,j))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_particiones :: [Int] -> Positive Int -> Bool
prop_particiones xs (Positive k) =
  all (iguales (particiones1 xs' k))
      [particiones2 xs' k,
       particiones3 xs' k]
  where
    xs' = nub xs
    iguales xss yss = sort (map sort [map sort x | x <- xss]) ==
                      sort (map sort [map sort y | y <- yss])

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_particiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (particiones1 [1..12] 6)
--    1323652
--    (1.33 secs, 1,152,945,584 bytes)
--    λ> length (particiones2 [1..12] 6)
--    1323652
--    (1.07 secs, 1,104,960,360 bytes)
--    λ> length (particiones3 [1..12] 6)
--    1323652
--    (1.68 secs, 1,047,004,368 bytes)

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en "Count number of ways to partition a set into k subsets"
-- http://bit.ly/1MNQkSo
