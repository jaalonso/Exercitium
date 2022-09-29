-- Puntos_dentro_del_circulo.hs
-- Puntos dentro del círculo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En el círculo de radio 2 hay 6 puntos cuyas coordenadas son puntos
-- naturales:
--    (0,0),(0,1),(0,2),(1,0),(1,1),(2,0)
-- y en de radio 3 hay 11:
--    (0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),(3,0)
--
-- Definir la función
--    circulo :: Int -> Int
-- tal que (circulo n) es el la cantidad de pares de números naturales
-- (x,y) que se encuentran en el círculo de radio n. Por ejemplo,
--    circulo 1    ==  3
--    circulo 2    ==  6
--    circulo 3    ==  11
--    circulo 4    ==  17
--    circulo 100  ==  7955
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Puntos_dentro_del_circulo where

import Test.QuickCheck

-- 1ª solución
-- ===========

circulo1 :: Int -> Int
circulo1 n = length (enCirculo1 n)

enCirculo1 :: Int -> [(Int, Int)]
enCirculo1 n = [(x,y) | x <- [0..n],
                        y <- [0..n],
                        x*x+y*y <= n*n]

-- 2ª solución
-- ===========

circulo2 :: Int -> Int
circulo2 0 = 1
circulo2 n =
  2 * length (enSemiCirculo n) + ceiling(fromIntegral n / sqrt 2)

enSemiCirculo :: Int -> [(Int, Int)]
enSemiCirculo n =
  [(x,y) | x <- [0..floor (sqrt (fromIntegral (n * n)))],
           y <- [x+1..truncate (sqrt (fromIntegral (n*n - x*x)))]]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_circulo :: Positive Int -> Bool
prop_circulo (Positive n) =
  circulo1 n == circulo2 n

-- La comprobación es
--    λ> quickCheck prop_circulo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> circulo1 (2*10^3)
--    3143587
--    (3.58 secs, 1,744,162,600 bytes)
--    λ> circulo2 (2*10^3)
--    3143587
--    (0.41 secs, 266,374,208 bytes)
