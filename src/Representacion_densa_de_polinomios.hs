-- Representacion_densa_de_polinomios.hs
-- Representación densa de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los polinomios pueden representarse de forma dispersa o densa. Por
-- ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar de forma
-- dispersa por [6,0,-5,4,-7] y de forma densa por [(4,6),(2,-5),(1,4),(0,-7)].
--
-- Definir la función
--    densa :: [Int] -> [(Int,Int)]
-- tal que (densa xs) es la representación densa del polinomio cuya
-- representación dispersa es xs. Por ejemplo,
--   densa [6,0,-5,4,-7]  ==  [(4,6),(2,-5),(1,4),(0,-7)]
--   densa [6,0,0,3,0,4]  ==  [(5,6),(2,3),(0,4)]
--   densa [0]            ==  [(0,0)]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Representacion_densa_de_polinomios where

import Test.QuickCheck

-- 1ª solución
-- ===========

densa1 :: [Int] -> [(Int,Int)]
densa1 xs =
  [(x,y) | (x,y) <- zip [n-1,n-2..1] xs, y /= 0]
  ++ [(0, last xs)]
  where n = length xs

-- 2ª solución
-- ===========

densa2 :: [Int] -> [(Int,Int)]
densa2 xs =
  filter (\ (_,y) -> y /= 0) (zip [n-1,n-2..1] xs)
  ++ [(0, last xs)]
  where n = length xs

-- 3ª solución
-- ===========

densa3 :: [Int] -> [(Int,Int)]
densa3 xs = filter ((/= 0) . snd) (zip [n-1,n-2..1] xs)
  ++ [(0, last xs)]
  where n = length xs

-- 4ª solución
-- ===========

densa4 :: [Int] -> [(Int,Int)]
densa4 xs = aux xs (length xs - 1)
  where aux [y] 0 = [(0, y)]
        aux (y:ys) n | y == 0    = aux ys (n-1)
                     | otherwise = (n,y) : aux ys (n-1)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_densa :: NonEmptyList Int -> Bool
prop_densa (NonEmpty xs) =
  all (== densa1 xs)
      [densa2 xs,
       densa3 xs,
       densa4 xs]

-- La comprobación es
--    λ> quickCheck prop_densa
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (densa1 [1..2*10^6])
--    (0,2000000)
--    (0.95 secs, 880,569,400 bytes)
--    λ> last (densa2 [1..2*10^6])
--    (0,2000000)
--    (0.52 secs, 800,569,432 bytes)
--    λ> last (densa3 [1..2*10^6])
--    (0,2000000)
--    (0.53 secs, 752,569,552 bytes)
--    λ> last (densa4 [1..2*10^6])
--    (0,2000000)
--    (3.05 secs, 1,267,842,032 bytes)
--
--    λ> last (densa1 [1..10^7])
--    (0,10000000)
--    (5.43 secs, 4,400,570,128 bytes)
--    λ> last (densa2 [1..10^7])
--    (0,10000000)
--    (3.03 secs, 4,000,570,160 bytes)
--    λ> last (densa3 [1..10^7])
--    (0,10000000)
--    (2.34 secs, 3,760,570,280 bytes)
