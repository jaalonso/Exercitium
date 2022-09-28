-- Puntos_dentro_del_circulo.hs
-- Puntos dentro del círculo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    circulo :: Int -> Int
-- tal que (circulo n) es el la cantidad de pares de números naturales
-- (x,y) que se encuentran dentro del círculo de radio n. Por ejemplo,
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
--    circulo 100  ==  7949
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Puntos_dentro_del_circulo where

import Test.QuickCheck

-- 1ª solución
-- ===========

circulo1 :: Int -> Int
circulo1 n = length [(x,y) | x <- [0..n], y <- [0..n],
                             x*x+y*y < n*n]

-- 2ª solución
-- ===========

circulo2 :: Int -> Int
circulo2 n = length [(x,y) | x <- [0..n-1]
                           , y <- [0..raizCuadradaEntera (n*n - x*x)]
                           , x*x+y*y < n*n]

-- (raizCuadradaEntera n) es la parte entera de la raíz cuadrada de
-- n. Por ejemplo,
--    raizCuadradaEntera 17  ==  4
raizCuadradaEntera :: Int -> Int
raizCuadradaEntera n = truncate (sqrt (fromIntegral n))

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
--    3143579
--    (4.51 secs, 1,744,161,832 bytes)
--    λ> circulo2 (2*10^3)
--    3143579
--    (2.85 secs, 1,435,776,200 bytes)
