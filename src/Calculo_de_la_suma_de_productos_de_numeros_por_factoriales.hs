-- Calculo_de_la_suma_de_productos_de_numeros_por_factoriales.hs
-- Cálculo de la suma 1*1! + 2*2! + 3*3! + ... + n*n!
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    suma :: Integer -> Integer
-- tal que (suma n) es la suma 1*1! + 2*2! + 3*3! + ... + n*n!. Por
-- ejemplo,
--    suma 1  ==  1
--    suma 2  ==  5
--    suma 3  ==  23
--    suma 4  ==  119
--    suma 5  ==  719
--    take 9 (show (suma 70000))  ==  "823780458"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Calculo_de_la_suma_de_productos_de_numeros_por_factoriales where

import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

suma1 :: Integer -> Integer
suma1 n = sum [k * factorial k | k <- [1..n]]

factorial :: Integer -> Integer
factorial n = product [1..n]

-- 2ª solución
-- ===========

suma2 :: Integer -> Integer
suma2 n = sum (zipWith (*) [1..n] factoriales)

factoriales :: [Integer]
factoriales = scanl (*) 1 [2..]

-- 3ª solución
-- ===========

-- Basada en los siguientes cálculos
--    λ> [suma1 n | n <- [0..10]]
--    [0,1,5,23,119,719,5039,40319,362879,3628799,39916799]
--    λ> [factorial n | n <- [0..10]]
--    [1,1,2,6,24,120,720,5040,40320,362880,3628800]
--    λ> [factorial n | n <- [1..11]]
--    [1,2,6,24,120,720,5040,40320,362880,3628800,39916800]
--    λ> [factorial n - 1 | n <- [1..11]]
--    [0,1,5,23,119,719,5039,40319,362879,3628799,39916799]

suma3 :: Integer -> Integer
suma3 n = factorial (n+1) - 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_suma :: Positive Integer -> Bool
prop_suma (Positive n) =
  all (== suma1 n)
      [suma2 n,
       suma3 n]
  
-- La comprobación es
--    λ> quickCheck prop_suma
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> take 5 (show (suma1 4000))
--    "73170"
--    (5.04 secs, 16,225,195,448 bytes)
--    λ> take 5 (show (suma2 4000))
--    "73170"
--    (0.08 secs, 35,862,152 bytes)
--    λ> take 5 (show (suma3 4000))
--    "73170"
--    (0.01 secs, 12,896,968 bytes)
--    
--    
--    λ> take 5 (show (suma2 40000))
--    "83669"
--    (1.82 secs, 4,549,612,264 bytes)
--    λ> take 5 (show (suma3 40000))
--    "83669"
--    (0.24 secs, 1,620,976,984 bytes)
