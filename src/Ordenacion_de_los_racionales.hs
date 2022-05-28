-- Ordenacion_de_los_racionales.hs
-- Ordenación de los racionales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En este ejercicio, representamos las fracciones mediante pares de
-- números de enteros. 
-- 
-- Definir la función 
--    fraccionesOrd :: Integer -> [(Integer,Integer)]
-- tal que (fraccionesOrd n) es la lista con las fracciones propias
-- positivas ordenadas, con denominador menor o igual que n. Por
-- ejemplo,
--    λ> fraccionesOrd 4
--    [(1,4),(1,3),(1,2),(2,3),(3,4)]
--    λ> fraccionesOrd 5
--    [(1,5),(1,4),(1,3),(2,5),(1,2),(3,5),(2,3),(3,4),(4,5)]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Ordenacion_de_los_racionales where

import Data.List (sort, sortBy)
import Data.Ratio ((%), numerator, denominator)
import Test.QuickCheck

-- 1ª solución
-- ===========

fraccionesOrd1 :: Integer -> [(Integer,Integer)]
fraccionesOrd1 n = 
  [(x,y) | (_,(x,y)) <- sort [(fromIntegral x/fromIntegral y,(x,y))
                              | y <- [2..n], 
                                x <- [1..y-1], 
                                gcd x y == 1]]

-- 2ª solución
-- ===========

fraccionesOrd2 :: Integer -> [(Integer,Integer)]
fraccionesOrd2 n = 
  map snd (sort [(fromIntegral x/fromIntegral y,(x,y))
                 | y <- [2..n], 
                   x <- [1..y-1], 
                   gcd x y == 1])

-- 3ª solución
-- ===========

fraccionesOrd3 :: Integer -> [(Integer,Integer)]
fraccionesOrd3 n = 
  sortBy comp [(x,y) | y <- [2..n], x <- [1..y-1], gcd x y == 1]
  where comp (a,b) (c,d) = compare (a*d) (b*c)

-- 4ª solución
-- ===========

fraccionesOrd4 :: Integer -> [(Integer,Integer)]
fraccionesOrd4 n = 
  [(numerator x, denominator x) | x <- racionalesOrd4 n]
  
-- (racionalesOrd4 n) es la lista con los racionales ordenados, con
-- denominador menor o igual que n. Por ejemplo,
--    λ> racionalesOrd4 4
--    [1 % 4,1 % 3,1 % 2,2 % 3,3 % 4]
racionalesOrd4 :: Integer -> [Rational]
racionalesOrd4 n =
  sort [x % y | y <- [2..n], x <- [1..y-1], gcd x y == 1]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_fraccionesOrd :: Positive Integer -> Bool 
prop_fraccionesOrd (Positive n) =
  all (== fraccionesOrd1 n)
      [fraccionesOrd2 n,
       fraccionesOrd3 n,
       fraccionesOrd4 n]

-- La comprobación es
--    λ> quickCheck prop_fraccionesOrd
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (fraccionesOrd1 2000)
--    1216587
--    (3.65 secs, 2,879,842,368 bytes)
--    λ> length (fraccionesOrd2 2000)
--    1216587
--    (3.36 secs, 2,870,109,640 bytes)
--    λ> length (fraccionesOrd3 2000)
--    1216587
--    (8.83 secs, 5,700,519,584 bytes)
--    λ> length (fraccionesOrd4 2000)
--    1216587
--    (4.12 secs, 5,181,904,336 bytes)

