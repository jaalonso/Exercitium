-- Numeracion_con_multiples_base.hs
-- Numeración con múltiples bases
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Sea (b(i) | i ≥ 1) una sucesión infinita de números enteros mayores
-- que 1. Entonces todo entero x mayor que cero se puede escribir de
-- forma única como
--    x = x(0) + x(1)b(1) +x(2)b(1)b(2) + ... + x(n)b(1)b(2)...b(n)
-- donde cada x(i) satisface la condición 0 ≤ x(i) < b(i+1). Se dice
-- que [x(n),x(n-1),...,x(2),x(1),x(0)] es la representación de x en la
-- base (b(i)). Por ejemplo, la representación de 377 en la base
-- (2*i | i >= 1) es [7,5,0,1] ya que
--    377 = 1 + 0*2 + 5*2*4 + 7*2*4*6
-- y, además, 0 ≤ 1 < 2, 0 ≤ 0 < 4, 0 ≤ 5 < 6 y 0 ≤ 7 < 8.
--
-- Definir las funciones
--    decimalAmultiple :: [Integer] -> Integer -> [Integer]
--    multipleAdecimal :: [Integer] -> [Integer] -> Integer
-- tales que
-- + (decimalAmultiple bs x) es la representación del número x en la
--   base bs. Por ejemplo,
--      decimalAmultiple [2,4..] 377                      ==  [7,5,0,1]
--      decimalAmultiple [2,5..] 377                      ==  [4,5,3,1]
--      decimalAmultiple [2^n | n <- [1..]] 2015          ==  [1,15,3,3,1]
--      decimalAmultiple (repeat 10) 2015                 ==  [2,0,1,5]
-- + (multipleAdecimal bs cs) es el número decimal cuya  representación
--   en la base bs es cs. Por ejemplo,
--      multipleAdecimal [2,4..] [7,5,0,1]                ==  377
--      multipleAdecimal [2,5..] [4,5,3,1]                ==  377
--      multipleAdecimal [2^n | n <- [1..]] [1,15,3,3,1]  ==  2015
--      multipleAdecimal (repeat 10) [2,0,1,5]            ==  2015
--
-- Comprobar con QuickCheck que se verifican las siguientes propiedades
-- + Para cualquier base bs y cualquier entero positivo n,
--      multipleAdecimal bs (decimalAmultiple bs x) == x
-- + Para cualquier base bs y cualquier entero positivo n, el coefiente
--   i-ésimo de la representación múltiple de n en la base bs es un
--   entero no negativo menos que el i-ésimo elemento de bs.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Numeracion_con_multiples_base where

import Test.QuickCheck
import Data.List (unfoldr)

-- 1ª solución de decimalAmultiple
-- ===============================

decimalAmultiple1 :: [Integer] -> Integer -> [Integer]
decimalAmultiple1 bs n = reverse (aux bs n)
  where aux _ 0      = []
        aux (d:ds) m = r : aux ds q
          where (q,r) = quotRem m d

-- 2ª solución de decimalAmultiple
-- ===============================

decimalAmultiple2 :: [Integer] -> Integer -> [Integer]
decimalAmultiple2 bs n = aux bs n []
  where aux _ 0  xs     = xs
        aux (d:ds) m xs = aux ds q (r:xs)
          where (q,r) = quotRem m d

-- 3ª solución de decimalAmultiple
-- ===============================

decimalAmultiple3 :: [Integer] -> Integer -> [Integer]
decimalAmultiple3 xs n = reverse (unfoldr f (xs,n))
  where f (_     ,0) = Nothing
        f ((y:ys),m) = Just (r,(ys,q))
                       where (q,r) = quotRem m y

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_decimalAmultiple :: InfiniteList (Positive Integer) -> Positive Integer -> Bool
prop_decimalAmultiple (InfiniteList xs _) (Positive n) =
  all (== decimalAmultiple1 xs' n)
      [decimalAmultiple2 xs' n,
       decimalAmultiple3 xs' n]
  where xs' = map getPositive xs

-- Comparación de eficiencia de decimalAmultiple
-- =============================================

-- La comparación es
--    λ> length (decimalAmultiple1 [2,7..] (10^(10^5)))
--    21731
--    (0.45 secs, 486,085,256 bytes)
--    λ> length (decimalAmultiple2 [2,7..] (10^(10^5)))
--    21731
--    (0.32 secs, 485,563,664 bytes)
--    λ> length (decimalAmultiple3 [2,7..] (10^(10^5)))
--    21731
--    (0.44 secs, 487,649,768 bytes)

-- 1ª solución de multipleAdecimal
-- ===============================

multipleAdecimal1  :: [Integer] -> [Integer] -> Integer
multipleAdecimal1 xs ns = aux xs (reverse ns)
  where aux (y:ys) (m:ms) = m + y * (aux ys ms)
        aux _ _           = 0

-- 2ª solución de multipleAdecimal
-- ===============================

multipleAdecimal2 :: [Integer] -> [Integer] -> Integer
multipleAdecimal2 bs xs =
  sum (zipWith (*) (reverse xs) (1 : scanl1 (*) bs))

-- Comprobación de equivalencia de multipleAdecimal
-- ================================================

-- La propiedad es
prop_multipleAdecimal :: InfiniteList (Positive Integer) -> [Positive Integer] -> Bool
prop_multipleAdecimal (InfiniteList xs _) ys =
  multipleAdecimal1 xs' ys' == multipleAdecimal2 xs' ys'
  where xs' = map getPositive xs
        ys' = map getPositive ys

-- La comprobación es
--    λ> quickCheck prop_multipleAdecimal
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de multipleAdecimal
-- =============================================

-- La comparación es
--    λ> length (show (multipleAdecimal1 [2,3..] [1..10^4]))
--    35660
--    (0.14 secs, 179,522,152 bytes)
--    λ> length (show (multipleAdecimal2 [2,3..] [1..10^4]))
--    35660
--    (0.22 secs, 243,368,664 bytes)

-- Comprobación de las propiedades
-- ===============================

-- La primera propiedad es
prop_inversas :: InfiniteList (Positive Integer) -> Positive Integer -> Bool
prop_inversas (InfiniteList xs _) (Positive n) =
  multipleAdecimal1 xs' (decimalAmultiple1 xs' n) == n
  where xs' = map getPositive xs

-- Su comprobación es
--    λ> quickCheck prop_inversas
--    +++ OK, passed 100 tests.

-- la 2ª propiedad es
prop_coeficientes :: InfiniteList (Positive Integer) -> Positive Integer -> Bool
prop_coeficientes (InfiniteList xs _) (Positive n) =
  and [0 <= c && c < b | (c,b) <- zip cs xs']
  where xs' = map getPositive xs
        cs = reverse (decimalAmultiple1 xs' n)

-- Su comprobación es
--    λ> quickCheck prop_coeficientes
--    +++ OK, passed 100 tests.
