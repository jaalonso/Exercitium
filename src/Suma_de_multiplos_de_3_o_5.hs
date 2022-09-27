-- Suma_de_multiplos_de_3_o_5.hs
-- Suma de múltiplos de 3 ó 5.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    euler1 :: Integer -> Integer
-- tal que (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores
-- que n. Por ejemplo,
--    euler1 10      == 23
--    euler1 (10^2)  == 2318
--    euler1 (10^3)  == 233168
--    euler1 (10^4)  == 23331668
--    euler1 (10^5)  == 2333316668
--    euler1 (10^10) == 23333333331666666668
--    euler1 (10^20) == 2333333333333333333316666666666666666668
--    euler1 (10^30) == 233333333333333333333333333333166666666666666666666666666668
--
-- Nota: Este ejercicio está basado en el problema 1 del Proyecto Euler
-- https://projecteuler.net/problem=1
-- ---------------------------------------------------------------------

module Suma_de_multiplos_de_3_o_5 where

import Data.List (nub, union)
import qualified Data.Set as S (fromAscList, union)
import Test.QuickCheck

-- 1ª solución
-- ===========

euler1a :: Integer -> Integer
euler1a n =
  sum [x | x <- [1..n-1],
           multiplo x 3 || multiplo x 5]

-- (multiplo x y) se verifica si x es un múltiplo de y. Por ejemplo.
--    multiplo 12 3  ==  True
--    multiplo 14 3  ==  False
multiplo :: Integer -> Integer -> Bool
multiplo x y = mod x y == 0

-- 2ª solución                                                        --
-- ===========

euler1b :: Integer -> Integer
euler1b n =
  sum [x | x <- [1..n-1],
           gcd x 15 > 1]

-- 3ª solución                                                        --
-- ===========

euler1c :: Integer -> Integer
euler1c n =
  sum [3,6..n-1] + sum [5,10..n-1] - sum [15,30..n-1]

-- 4ª solución                                                        --
-- ===========

euler1d :: Integer -> Integer
euler1d n =
  sum (nub ([3,6..n-1] ++ [5,10..n-1]))

-- 5ª solución                                                        --
-- ===========

euler1e :: Integer -> Integer
euler1e n =
  sum ([3,6..n-1] `union` [5,10..n-1])

-- 6ª solución                                                        --
-- ===========

euler1f :: Integer -> Integer
euler1f n =
  sum (S.fromAscList [3,6..n-1] `S.union` S.fromAscList [5,10..n-1])

-- 7ª solución                                                      --
-- ===========

euler1g :: Integer -> Integer
euler1g n =
  suma 3 n + suma 5 n - suma 15 n

-- (suma d x) es la suma de los múltiplos de p menores que x. Por
-- ejemplo,

suma :: Integer -> Integer -> Integer
suma d x = (a+b)*n `div` 2
    where a = d
          b = d * ((x-1) `div` d)
          n = 1 + (b-a) `div` d

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_euler1 :: Positive Integer -> Bool
prop_euler1 (Positive n) =
  all (== euler1a n)
      [euler1b n,
       euler1c n,
       euler1d n,
       euler1e n,
       euler1f n,
       euler1g n]

-- La comprobación es
--    λ> quickCheck prop_euler1
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> euler1a (5*10^4)
--    583291668
--    (0.05 secs, 21,895,296 bytes)
--    λ> euler1b (5*10^4)
--    583291668
--    (0.05 secs, 26,055,096 bytes)
--    λ> euler1c (5*10^4)
--    583291668
--    (0.01 secs, 5,586,072 bytes)
--    λ> euler1d (5*10^4)
--    583291668
--    (2.83 secs, 7,922,304 bytes)
--    λ> euler1e (5*10^4)
--    583291668
--    (4.56 secs, 12,787,705,248 bytes)
--    λ> euler1f (5*10^4)
--    583291668
--    (0.01 secs, 8,168,584 bytes)
--    λ> euler1g (5*10^4)
--    583291668
--    (0.02 secs, 557,488 bytes)
--
--    λ> euler1a (3*10^6)
--    2099998500000
--    (2.72 secs, 1,282,255,816 bytes)
--    λ> euler1b (3*10^6)
--    2099998500000
--    (2.06 secs, 1,531,855,776 bytes)
--    λ> euler1c (3*10^6)
--    2099998500000
--    (0.38 secs, 305,127,480 bytes)
--    λ> euler1f (3*10^6)
--    2099998500000
--    (0.54 secs, 457,358,232 bytes)
--    λ> euler1g (3*10^6)
--    2099998500000
--    (0.01 secs, 560,472 bytes)
--
--    λ> euler1c (10^7)
--    23333331666668
--    (1.20 secs, 1,015,920,024 bytes)
--    λ> euler1f (10^7)
--    23333331666668
--    (2.00 secs, 1,523,225,648 bytes)
--    λ> euler1g (10^7)
--    23333331666668
--    (0.01 secs, 561,200 bytes)
