-- Suma_de_multiplos_de_3_o_de_5.hs
-- Suma de múltiplos de 3 o de 5
-- José A. Alonso Jiménez
-- Sevilla, 9-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números naturales menores que 10 que son múltiplos de 3 ó 5 son
-- 3, 5, 6 y 9. La suma de estos múltiplos es 23.
--
-- Definir la función
--    sumaMultiplos :: Integer -> Integer
-- tal que (sumaMultiplos n) es la suma de todos los múltiplos de 3 ó 5
-- menores que n. Por ejemplo,
--    sumaMultiplos 10      ==  23
--    sumaMultiplos (10^2)  ==  2318
--    sumaMultiplos (10^3)  ==  233168
--    sumaMultiplos (10^4)  ==  23331668
--    sumaMultiplos (10^5)  ==  2333316668
--    sumaMultiplos (10^6)  ==  233333166668
--    sumaMultiplos (10^7)  ==  23333331666668
--
-- Referencia: Basado en el
-- [problema 1 del proyecto Euler](https://projecteuler.net/problem=1).
-- ---------------------------------------------------------------------

module Suma_de_multiplos_de_3_o_de_5 where

import Data.List (nub, union)
import Test.QuickCheck (Positive(..), quickCheck)

-- 1ª solución
-- ===========

sumaMultiplos1 :: Integer -> Integer
sumaMultiplos1 n = sum [x | x <- [1..n-1], multiplo x 3 || multiplo x 5]

-- (multiplo x y) se verifica si x es múltiplo de y. Por ejemplo,
--    multiplo 6 3  ==  True
--    multiplo 6 4  ==  False
multiplo :: Integer -> Integer -> Bool
multiplo x y = mod x y == 0

-- 2ª solución                                                        --
-- ===========

sumaMultiplos2 :: Integer -> Integer
sumaMultiplos2 n = sum [x | x <- [1..n-1], gcd x 15 > 1]

-- 3ª solución                                                        --
-- ===========

sumaMultiplos3 :: Integer -> Integer
sumaMultiplos3 n = sum [3,6..n-1] + sum [5,10..n-1] - sum [15,30..n-1]

-- 4ª solución                                                        --
-- ===========

sumaMultiplos4 :: Integer -> Integer
sumaMultiplos4 n = sum (nub ([3,6..n-1] ++ [5,10..n-1]))

-- 5ª solución                                                        --
-- ===========

sumaMultiplos5 :: Integer -> Integer
sumaMultiplos5 n = sum ([3,6..n-1] `union` [5,10..n-1])

-- 6ª solución                                                      --
-- ===========

sumaMultiplos6 :: Integer -> Integer
sumaMultiplos6 n = suma 3 n + suma 5 n - suma 15 n

-- (suma d x) es la suma de los múltiplos de d menores que x. Por
-- ejemplo,
--    suma 3 10  ==  18
--    suma 5 10  ==  5
suma :: Integer -> Integer -> Integer
suma d x = (a+b)*n `div` 2
    where a = d
          b = d * ((x-1) `div` d)
          n = 1 + (b-a) `div` d

-- Equivalencia de definiciones
-- ============================

-- La propiedad es
prop_sumaMultiplos :: Positive Integer -> Bool
prop_sumaMultiplos (Positive n) =
  all (== (sumaMultiplos1 n))
      [f n | f <- [sumaMultiplos1,
                   sumaMultiplos2,
                   sumaMultiplos3,
                   sumaMultiplos4,
                   sumaMultiplos5,
                   sumaMultiplos6]]

verifica_sumaMultiplos :: IO ()
verifica_sumaMultiplos =
  quickCheck prop_sumaMultiplos

-- La comprobación es
--    λ> verifica_sumaMultiplos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaMultiplos1 (5*10^4)
--    583291668
--    (0.05 secs, 21,446,456 bytes)
--    λ> sumaMultiplos2 (5*10^4)
--    583291668
--    (0.05 secs, 26,804,944 bytes)
--    λ> sumaMultiplos3 (5*10^4)
--    583291668
--    (0.01 secs, 5,136,728 bytes)
--    λ> sumaMultiplos4 (5*10^4)
--    583291668
--    (3.05 secs, 7,474,304 bytes)
--    λ> sumaMultiplos5 (5*10^4)
--    583291668
--    (5.14 secs, 12,787,717,152 bytes)
--    λ> sumaMultiplos6 (5*10^4)
--    583291668
--    (0.01 secs, 108,448 bytes)
--
--    λ> sumaMultiplos1 (3*10^6)
--    2099998500000
--    (2.14 secs, 1,281,805,696 bytes)
--    λ> sumaMultiplos2 (3*10^6)
--    2099998500000
--    (1.86 secs, 1,603,407,272 bytes)
--    λ> sumaMultiplos3 (3*10^6)
--    2099998500000
--    (0.39 secs, 304,681,080 bytes)
--    λ> sumaMultiplos6 (3*10^6)
--    2099998500000
--    (0.01 secs, 112,544 bytes)
--
--    λ> sumaMultiplos3 (10^7)
--    23333331666668
--    (1.69 secs, 1,015,468,352 bytes)
--    λ> sumaMultiplos6 (10^7)
--    23333331666668
--    (0.01 secs, 112,336 bytes)
