-- Separacion_por_posicion.hs
-- Separación por posición.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    particion :: [a] -> ([a],[a])
-- tal que (particion xs) es el par cuya primera componente son los
-- elementos de xs en posiciones pares y su segunda componente son los
-- restantes elementos. Por ejemplo,
--    particion [3,5,6,2]    ==  ([3,6],[5,2])
--    particion [3,5,6,2,7]  ==  ([3,6,7],[5,2])
--    particion "particion"  ==  ("priin","atco")
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Separacion_por_posicion where

import Data.List (partition)
import qualified Data.Vector as V
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

particion1 :: [a] -> ([a],[a])
particion1 xs = ([x | (n,x) <- nxs, even n],
                 [x | (n,x) <- nxs, odd n])
  where nxs = enumeracion xs

--(numeracion xs) es la enumeración de xs. Por ejemplo,
--    enumeracion [7,9,6,8]  ==  [(0,7),(1,9),(2,6),(3,8)]
enumeracion :: [a] -> [(Int,a)]
enumeracion = zip [0..]

-- 2ª solución
-- ===========

particion2 :: [a] -> ([a],[a])
particion2 []     = ([],[])
particion2 (x:xs) = (x:zs,ys)
  where (ys,zs) = particion2 xs

-- 3ª solución
-- ===========

particion3 :: [a] -> ([a],[a])
particion3 = foldr f ([],[])
  where f x (ys,zs) = (x:zs,ys)

-- 4ª solución
-- ===========

particion4 :: [a] -> ([a],[a])
particion4 = foldr (\x (ys,zs) -> (x:zs,ys)) ([],[])

-- 5ª solución
-- ===========

particion5 :: [a] -> ([a],[a])
particion5 xs =
  ([xs!!k | k <- [0,2..n]],
   [xs!!k | k <- [1,3..n]])
  where n = length xs - 1

-- 6ª solución
-- ===========

particion6 :: [a] -> ([a],[a])
particion6 xs = (pares xs, impares xs)

-- (pares xs) es la lista de los elementos de xs en posiciones
-- pares. Por ejemplo,
--    pares [3,5,6,2]  ==  [3,6]
pares :: [a] -> [a]
pares []     = []
pares (x:xs) = x : impares xs

-- (impares xs) es la lista de los elementos de xs en posiciones
-- impares. Por ejemplo,
--    impares [3,5,6,2]  ==  [5,2]
impares :: [a] -> [a]
impares []     = []
impares (_:xs) = pares xs

-- 7ª solución
-- ===========

particion7 :: [a] -> ([a],[a])
particion7 [] = ([],[])
particion7 xs =
  ([v V.! k | k <- [0,2..n-1]],
   [v V.! k | k <- [1,3..n-1]])
  where v = V.fromList xs
        n = V.length v

-- 8ª solución
-- ===========

particion8 :: [a] -> ([a],[a])
particion8 xs =
  (map snd ys, map snd zs)
  where (ys,zs) = partition posicionPar (zip [0..] xs)

posicionPar :: (Int,a) -> Bool
posicionPar = even . fst

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_particion :: [Int] -> Bool
prop_particion xs =
  all (== particion1 xs)
      [particion2 xs,
       particion3 xs,
       particion4 xs,
       particion5 xs,
       particion6 xs,
       particion7 xs,
       particion8 xs]

-- La comprobación es
--    λ> quickCheck prop_particion
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (snd (particion1 [1..6*10^6]))
--    6000000
--    (2.74 secs, 2,184,516,080 bytes)
--    λ> last (snd (particion2 [1..6*10^6]))
--    6000000
--    (2.02 secs, 1,992,515,880 bytes)
--    λ> last (snd (particion3 [1..6*10^6]))
--    6000000
--    (3.17 secs, 1,767,423,240 bytes)
--    λ> last (snd (particion4 [1..6*10^6]))
--    6000000
--    (3.23 secs, 1,767,423,240 bytes)
--    λ> last (snd (particion5 [1..6*10^6]))
--    6000000
--    (1.62 secs, 1,032,516,192 bytes)
--    λ> last (snd (particion5 [1..6*10^6]))
--    6000000
--    (1.33 secs, 1,032,516,192 bytes)
--    λ> last (snd (particion6 [1..6*10^6]))
--    6000000
--    (1.80 secs, 888,515,960 bytes)
--    λ> last (snd (particion7 [1..6*10^6]))
--    6000000
--    (1.29 secs, 1,166,865,672 bytes)
--    λ> last (snd (particion8 [1..6*10^6]))
--    6000000
--    (0.87 secs, 3,384,516,616 bytes)
--
--    λ> last (snd (particion5 [1..10^7]))
--    10000000
--    (1.94 secs, 1,720,516,872 bytes)
--    λ> last (snd (particion7 [1..10^7]))
--    10000000
--    (2.54 secs, 1,989,215,176 bytes)
--    λ> last (snd (particion8 [1..10^7]))
--    10000000
--    (1.33 secs, 5,640,516,960 bytes)
