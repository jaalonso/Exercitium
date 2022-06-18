-- Aproximacion_de_numero_pi.hs
-- Aproximación del número pi.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una forma de aproximar el número π es usando la siguiente igualdad: 
--     π         1     1*2     1*2*3     1*2*3*4     
--    --- = 1 + --- + ----- + ------- + --------- + ....
--     2         3     3*5     3*5*7     3*5*7*9
-- Es decir, la serie cuyo término general n-ésimo es el cociente entre el
-- producto de los primeros n números y los primeros n números impares:
--                Π i   
--    s(n) =  -----------
--             Π (2*i+1)
--
-- Definir la función
--    aproximaPi :: Integer -> Double
-- tal que (aproximaPi n) es la aproximación del número π calculada con la
-- serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaPi 10     == 3.1411060206
--    aproximaPi 20     == 3.1415922987403397
--    aproximaPi 30     == 3.1415926533011596
--    aproximaPi 40     == 3.1415926535895466
--    aproximaPi 50     == 3.141592653589793
--    aproximaPi (10^4) == 3.141592653589793
--    pi                == 3.141592653589793
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Aproximacion_de_numero_pi where

import Data.Ratio ((%))
import Data.List (genericTake)
import Test.QuickCheck (Property, arbitrary, forAll, suchThat, quickCheck)

-- 1ª solución
-- ===========

aproximaPi1 :: Integer -> Double
aproximaPi1 n = 
  fromRational (2 * sum [product [1..i] % product [1,3..2*i+1] | i <- [0..n]])

-- 2ª solución
-- ===========

aproximaPi2 :: Integer -> Double
aproximaPi2 0 = 2
aproximaPi2 n = 
  aproximaPi2 (n-1) + fromRational (2 * product [1..n] % product [3,5..2*n+1])

-- 3ª solución
-- ===========

aproximaPi3 :: Integer -> Double
aproximaPi3 n = 
  fromRational (2 * (1 + sum (zipWith (%) numeradores (genericTake n denominadores))))

-- numeradores es la sucesión de los numeradores. Por ejemplo,
--    λ> take 10 numeradores
--    [1,2,6,24,120,720,5040,40320,362880,3628800]
numeradores :: [Integer]
numeradores = scanl (*) 1 [2..]

-- denominadores es la sucesión de los denominadores. Por ejemplo,
--    λ> take 10 denominadores
--    [3,15,105,945,10395,135135,2027025,34459425,654729075,13749310575]
denominadores :: [Integer]
denominadores = scanl (*) 3 [5, 7..]
 
-- 4ª solución
-- ===========

aproximaPi4 :: Integer -> Double
aproximaPi4 n = 
  read (x : "." ++ xs)
  where (x:xs) = show (aproximaPi4' n)

aproximaPi4' :: Integer -> Integer
aproximaPi4' n = 
  2 * (p + sum (zipWith div (map (*p) numeradores) (genericTake n denominadores))) 
  where p = 10^n

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_aproximaPi :: Property
prop_aproximaPi =
  forAll (arbitrary `suchThat` (> 3)) $ \n ->
  all (=~ aproximaPi1 n)
      [aproximaPi2 n,
       aproximaPi3 n,
       aproximaPi4 n]

(=~) :: Double -> Double -> Bool
x =~ y = abs (x - y) < 0.001

-- La comprobación es
--    λ> quickCheck prop_aproximaPi
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> aproximaPi1 3000 
--    3.141592653589793
--    (4.96 secs, 27,681,824,408 bytes)
--    λ> aproximaPi2 3000 
--    3.1415926535897922
--    (3.00 secs, 20,496,194,496 bytes)
--    λ> aproximaPi3 3000 
--    3.141592653589793
--    (3.13 secs, 13,439,528,432 bytes)
--    λ> aproximaPi4 3000 
--    3.141592653589793
--    (0.09 secs, 23,142,144 bytes)

