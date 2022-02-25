-- Primos_equidistantes.hs
-- Primos equidistantes.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    primosEquidistantes :: Integer -> [(Integer,Integer)]
-- tal que (primosEquidistantes k) es la lista de los pares de primos
-- cuya diferencia es k. Por ejemplo,
--    take 3 (primosEquidistantes 2)  ==  [(3,5),(5,7),(11,13)]
--    take 3 (primosEquidistantes 4)  ==  [(7,11),(13,17),(19,23)]
--    take 3 (primosEquidistantes 6)  ==  [(23,29),(31,37),(47,53)]
--    take 3 (primosEquidistantes 8)  ==  [(89,97),(359,367),(389,397)]
--    primosEquidistantes 4 !! (10^5) ==  (18467047,18467051)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Primos_equidistantes where

import Data.Numbers.Primes (primes)

-- 1ª solución
-- ===========

primosEquidistantes1 :: Integer -> [(Integer,Integer)]
primosEquidistantes1 k = aux primos
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]

-- 2ª solución
-- ===========

primosEquidistantes2 :: Integer -> [(Integer,Integer)]
primosEquidistantes2 k = aux primos2
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)

primos2 :: [Integer]
primos2 = criba [2..]
  where criba (p:ps) = p : criba [n | n <- ps, mod n p /= 0]

-- 3ª solución
-- ===========

primosEquidistantes3 :: Integer -> [(Integer,Integer)]
primosEquidistantes3 k = aux primos3
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)

primos3 :: [Integer]
primos3 = 2 : 3 : criba3 0 (tail primos3) 3
  where criba3 k (p:ps) x = [n | n <- [x+2,x+4..p*p-2],
                                 and [n `rem` q /= 0 | q <- take k (tail primos3)]]
                            ++ criba3 (k+1) ps (p*p)

-- 4ª solución
-- ===========

primosEquidistantes4 :: Integer -> [(Integer,Integer)]
primosEquidistantes4 k = aux primes
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)

-- 5ª solución
-- ===========

primosEquidistantes5 :: Integer -> [(Integer,Integer)]
primosEquidistantes5 k =
  [(x,y) | (x,y) <- zip primes (tail primes)
         , y - x == k]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosEquidistantes1 4 !! 200
--    (9829,9833)
--    (2.60 secs, 1,126,458,272 bytes)
--    λ> primosEquidistantes2 4 !! 200
--    (9829,9833)
--    (0.44 secs, 249,622,048 bytes)
--    λ> primosEquidistantes3 4 !! 200
--    (9829,9833)
--    (0.06 secs, 13,352,208 bytes)
--    λ> primosEquidistantes4 4 !! 200
--    (9829,9833)
--    (0.02 secs, 4,012,848 bytes)
--    λ> primosEquidistantes5 4 !! 200
--    (9829,9833)
--    (0.01 secs, 7,085,072 bytes)
--
--    λ> primosEquidistantes2 4 !! 600
--    (41617,41621)
--    (5.67 secs, 3,340,313,480 bytes)
--    λ> primosEquidistantes3 4 !! 600
--    (41617,41621)
--    (0.14 secs, 76,600,968 bytes)
--    λ> primosEquidistantes4 4 !! 600
--    (41617,41621)
--    (0.03 secs, 15,465,824 bytes)
--    λ> primosEquidistantes5 4 !! 600
--    (41617,41621)
--    (0.04 secs, 28,858,232 bytes)
--
--    λ> primosEquidistantes3 4 !! 5000
--    (556819,556823)
--    (3.58 secs, 2,040,940,144 bytes)
--    λ> primosEquidistantes4 4 !! 5000
--    (556819,556823)
--    (0.12 secs, 220,705,192 bytes)
--    λ> primosEquidistantes5 4 !! 5000
--    (556819,556823)
--    (0.16 secs, 424,501,800 bytes)
--
--    λ> primosEquidistantes4 4 !! (10^5)
--    (18467047,18467051)
--    (3.99 secs, 9,565,715,488 bytes)
--    λ> primosEquidistantes5 4 !! (10^5)
--    (18467047,18467051)
--    (7.95 secs, 18,712,469,144 bytes)
