-- Numeros_de_Pentanacci.hs
-- Números de Pentanacci.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-agosto-2022
-- ---------------------------------------------------------------------

-- --------------------------------------------------------------------- 
-- Los números de Fibonacci se definen mediante las ecuaciones
--    F(0) = 0
--    F(1) = 1
--    F(n) = F(n-1) + F(n-2), si n > 1
-- Los primeros números de Fibonacci son 
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, ...
-- 
-- Una generalización de los anteriores son los números de Pentanacci
-- definidos por las siguientes ecuaciones
--    P(0) = 0
--    P(1) = 1
--    P(2) = 1
--    P(3) = 2
--    P(4) = 4
--    P(n) = P(n-1) + P(n-2) + P(n-3) + P(n-4) + P(n-5), si n > 4
-- Los primeros números de Pentanacci son 
--   0, 1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, ...
-- 
-- Definir la sucesión
--    pentanacci :: [Integer]
-- cuyos elementos son los números de Pentanacci. Por ejemplo,
--    λ> take 15 pentanacci
--    [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]
--    λ> (pentanacci !! (10^5)) `mod` (10^30) 
--    482929150584077921552549215816
--    231437922897686901289110700696
--    λ> length (show (pentanacci !! (10^5)))
--    29357
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_de_Pentanacci where

import Data.List (zipWith5)
import Test.QuickCheck (NonNegative (NonNegative), quickCheckWith, maxSize, stdArgs)

-- 1ª solución
-- ===========

pentanacci1 :: [Integer]
pentanacci1 = [pent n | n <- [0..]]

pent :: Integer -> Integer
pent 0 = 0
pent 1 = 1
pent 2 = 1
pent 3 = 2
pent 4 = 4
pent n = pent (n-1) + pent (n-2) + pent (n-3) + pent (n-4) + pent (n-5)

-- 2ª solución
-- ===========

pentanacci2 :: [Integer]
pentanacci2 = 
  0 : 1 : 1 : 2 : 4 : zipWith5 f (r 0) (r 1) (r 2) (r 3) (r 4)
  where f a b c d e = a+b+c+d+e
        r n         = drop n pentanacci2

-- 3ª solución
-- ===========

pentanacci3 :: [Integer]
pentanacci3 = p (0, 1, 1, 2, 4)
  where p (a, b, c, d, e) = a : p (b, c, d, e, a + b + c + d + e)

-- 4ª solución
-- ===========

pentanacci4 :: [Integer]
pentanacci4 = 0: 1: 1: 2: 4: p pentanacci4
  where p (a:b:c:d:e:xs) = (a+b+c+d+e): p (b:c:d:e:xs)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_pentanacci :: NonNegative Int -> Bool
prop_pentanacci (NonNegative n) =
  all (== pentanacci1 !! n)
      [pentanacci1 !! n,
       pentanacci2 !! n,
       pentanacci3 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=25}) prop_pentanacci
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> pentanacci1 !! 25
--    5976577
--    (3.18 secs, 1,025,263,896 bytes)
--    λ> pentanacci2 !! 25
--    5976577
--    (0.00 secs, 562,360 bytes)
--    
--    λ> length (show (pentanacci2 !! (10^5)))
--    29357
--    (1.04 secs, 2,531,259,408 bytes)
--    λ> length (show (pentanacci3 !! (10^5)))
--    29357
--    (1.00 secs, 2,548,868,384 bytes)
--    λ> length (show (pentanacci4 !! (10^5)))
--    29357
--    (0.96 secs, 2,580,065,520 bytes)

-- Referencias
-- ===========

-- + Tito III Piezas y Eric Weisstein, [Pentanacci number](https://bit.ly/3cPJGkF).
-- + N. J. A. Sloane, [Sucesión A001591 de la OEIS](https://oeis.org/A001591).
