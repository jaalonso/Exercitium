-- Triangulo_aritmetico.hs
-- Triángulo aritmético
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los triángulos aritméticos se forman como sigue
--     1
--     2  3
--     4  5  6
--     7  8  9 10
--    11 12 13 14 15
--    16 17 18 19 20 21
--
-- Definir las funciones
--    linea     :: Integer -> [Integer]
--    triangulo :: Integer -> [[Integer]]
-- tales que
-- + (linea n) es la línea n-ésima de los triángulos aritméticos. Por
--   ejemplo,
--      linea 4  ==  [7,8,9,10]
--      linea 5  ==  [11,12,13,14,15]
--      head (linea (10^20)) == 4999999999999999999950000000000000000001
-- + (triangulo n) es el triángulo aritmético de altura n. Por ejemplo,
--      triangulo 3  ==  [[1],[2,3],[4,5,6]]
--      triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

module Triangulo_aritmetico where

import Test.QuickCheck

-- 1ª definición de línea
-- ======================

linea1 :: Integer -> [Integer]
linea1 n = [suma1 (n-1)+1..suma1 n]

-- (suma n) es la suma de los n primeros números. Por ejemplo,
--    suma 3  ==  6
suma1 :: Integer -> Integer
suma1 n = sum [1..n]

-- 2ª definición de línea
-- ======================

linea2 :: Integer -> [Integer]
linea2 n = [s+1..s+n]
  where s = suma1 (n-1)

-- 3ª definición de línea
-- ======================

linea3 :: Integer -> [Integer]
linea3 n = [s+1..s+n]
  where s = suma2 (n-1)

suma2 :: Integer -> Integer
suma2 n = (1+n)*n `div` 2

-- Comprobación de equivalencia de linea
-- =====================================

-- La propiedad es
prop_linea :: Positive Integer -> Bool
prop_linea (Positive n) =
  all (== linea1 n)
      [linea2 n,
       linea3 n]

-- La comprobación es
--    λ> quickCheck prop_linea
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de linea
-- ==================================

-- La comparación es
--    λ> last (linea1 (10^7))
--    50000005000000
--    (5.10 secs, 3,945,159,856 bytes)
--    λ> last (linea2 (10^7))
--    50000005000000
--    (3.11 secs, 2,332,859,512 bytes)
--    λ> last (linea3 (10^7))
--    50000005000000
--    (0.16 secs, 720,559,384 bytes)

-- 1ª definición de triangulo
-- ==========================

triangulo1 :: Integer -> [[Integer]]
triangulo1 n = [linea1 m | m <- [1..n]]

-- 2ª definición de triangulo
-- ==========================

triangulo2 :: Integer -> [[Integer]]
triangulo2 n = [linea2 m | m <- [1..n]]

-- 3ª definición de triangulo
-- ==========================

triangulo3 :: Integer -> [[Integer]]
triangulo3 n = [linea3 m | m <- [1..n]]

-- Comprobación de equivalencia de triangulo
-- =========================================

-- La propiedad es
prop_triangulo :: Positive Integer -> Bool
prop_triangulo (Positive n) =
  all (== triangulo1 n)
      [triangulo2 n,
       triangulo3 n]

-- La comprobación es
--    λ> quickCheck prop_triangulo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de triangulo
-- ======================================

-- La comparación es
--    λ> last (last (triangulo1 (3*10^6)))
--    4500001500000
--    (2.25 secs, 1,735,919,184 bytes)
--    λ> last (last (triangulo2 (3*10^6)))
--    4500001500000
--    (1.62 secs, 1,252,238,872 bytes)
--    λ> last (last (triangulo3 (3*10^6)))
--    4500001500000
--    (0.79 secs, 768,558,776 bytes)
