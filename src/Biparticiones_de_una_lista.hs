-- Biparticiones_de_una_lista.hs
-- Biparticiones de una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-Mayo-2014 (actualizado 28-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    biparticiones :: [a] -> [([a],[a])]
-- tal que (biparticiones xs) es la lista de pares formados por un
-- prefijo de xs y el resto de xs. Por ejemplo,
--    λ> biparticiones [3,2,5]
--    [([],[3,2,5]),([3],[2,5]),([3,2],[5]),([3,2,5],[])]
--    λ> biparticiones "Roma"
--    [("","Roma"),("R","oma"),("Ro","ma"),("Rom","a"),("Roma","")]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Biparticiones_de_una_lista where

import Data.List (inits, tails)
import Control.Applicative (liftA2)

import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

biparticiones1 :: [a] -> [([a],[a])]
biparticiones1 [] = [([],[])]
biparticiones1 (x:xs) =
  ([],x:xs) : [(x:ys,zs) | (ys,zs) <- biparticiones1 xs]

-- 2ª solución
-- ===========

biparticiones2 :: [a] -> [([a],[a])]
biparticiones2 xs =
  [(take i xs, drop i xs) | i <- [0..length xs]]

-- 3ª solución
-- ===========

biparticiones3 :: [a] -> [([a],[a])]
biparticiones3 xs =
  [splitAt i xs | i <- [0..length xs]]

-- 4ª solución
-- ===========

biparticiones4 :: [a] -> [([a],[a])]
biparticiones4 xs =
  zip (inits xs) (tails xs)

-- 5ª solución
-- ===========

biparticiones5 :: [a] -> [([a],[a])]
biparticiones5 = liftA2 zip inits tails

-- 6ª solución
-- ===========

biparticiones6 :: [a] -> [([a],[a])]
biparticiones6 = zip <$> inits <*> tails

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_biparticiones :: [Int] -> Bool
prop_biparticiones xs =
  all (== biparticiones1 xs)
      [biparticiones2 xs,
       biparticiones3 xs,
       biparticiones4 xs,
       biparticiones5 xs,
       biparticiones6 xs]

-- La comprobación es
--    λ> quickCheck prop_biparticiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (biparticiones1 [1..6*10^3])
--    6001
--    (2.21 secs, 3,556,073,552 bytes)
--    λ> length (biparticiones2 [1..6*10^3])
--    6001
--    (0.01 secs, 2,508,448 bytes)
--
--    λ> length (biparticiones2 [1..6*10^6])
--    6000001
--    (2.26 secs, 2,016,494,864 bytes)
--    λ> length (biparticiones3 [1..6*10^6])
--    6000001
--    (2.12 secs, 1,584,494,792 bytes)
--    λ> length (biparticiones4 [1..6*10^6])
--    6000001
--    (0.78 secs, 1,968,494,704 bytes)
--    λ> length (biparticiones5 [1..6*10^6])
--    6000001
--    (0.79 secs, 1,968,494,688 bytes)
--    λ> length (biparticiones6 [1..6*10^6])
--    6000001
--    (0.77 secs, 1,968,494,720 bytes)
--
--    λ> length (biparticiones4 [1..10^7])
--    10000001
--    (1.30 secs, 3,280,495,432 bytes)
--    λ> length (biparticiones5 [1..10^7])
--    10000001
--    (1.42 secs, 3,280,495,416 bytes)
--    λ> length (biparticiones6 [1..10^7])
--    10000001
--    (1.30 secs, 3,280,495,448 bytes)
