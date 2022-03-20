-- Regiones.hs
-- Regiones determinadas por n rectas del plano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En los siguientes dibujos se observa que el número máximo de regiones
-- en el plano generadas con 1, 2 ó 3 líneas son 2, 4 ó 7,
-- respectivamente.
--
--                       \  |
--                        \5|
--                         \|
--                          \
--                          |\
--                          | \
--                |         |  \
--     1        1 | 3     1 | 3 \  6
--    ------   ---|---   ---|----\---
--     2        2 | 4     2 | 4   \ 7
--                |         |      \
--
-- Definir la función
--    regiones :: Integer -> Integer
-- tal que (regiones n) es el número máximo de regiones en el plano
-- generadas con n líneas. Por ejemplo,
--    regiones 1     ==  2
--    regiones 2     ==  4
--    regiones 3     ==  7
--    regiones 100   ==  5051
--    regiones 1000  ==  500501
--    regiones 10000 ==  50005001
--    length (show (regiones (10^(10^5)))) ==  200000
--    length (show (regiones (10^(10^6)))) ==  2000000
--    length (show (regiones (10^(10^6)))) ==  2000000
--    length (show (regiones (10^(10^7)))) ==  20000000
-- ---------------------------------------------------------------------

module Regiones where

import Data.List (genericIndex)
import Test.QuickCheck

-- 1ª solución
-- ===========

regiones1 :: Integer -> Integer
regiones1 0 = 1
regiones1 n = regiones1 (n-1) + n

-- 2ª solución
-- ===========

regiones2 :: Integer -> Integer
regiones2 n = 1 + sum [0..n]

-- 3ª solución
-- ===========

regiones3 :: Integer -> Integer
regiones3 n = 1 + sumas `genericIndex` n

-- (sumas n) es la suma 0 + 1 + 2 +...+ n. Por ejemplo,
--    take 10 sumas  ==  [0,1,3,6,10,15,21,28,36,45]
sumas :: [Integer]
sumas = scanl1 (+) [0..]

-- 4ª solución
-- ===========

regiones4 :: Integer -> Integer
regiones4 n = 1 + n*(n+1) `div` 2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_regiones :: Positive Integer -> Bool
prop_regiones (Positive n) =
  all (== regiones1 n)
      [regiones2 n,
       regiones3 n,
       regiones4 n]

-- La comprobación es
--    λ> quickCheck prop_regiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> regiones1 (4*10^6)
--    8000002000001
--    (2.20 secs, 938,105,888 bytes)
--    λ> regiones2 (4*10^6)
--    8000002000001
--    (0.77 secs, 645,391,624 bytes)
--    λ> regiones3 (4*10^6)
--    8000002000001
--    (1.22 secs, 1,381,375,296 bytes)
--    λ> regiones4 (4*10^6)
--    8000002000001
--    (0.01 secs, 484,552 bytes)
--
