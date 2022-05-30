-- Termino_ausente_en_una_progresion_aritmetica.hs
-- Término ausente en una progresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una progresión aritmética es una sucesión de números tales que la
-- diferencia de dos términos sucesivos cualesquiera de la sucesión es
-- constante.
-- 
-- Definir la función 
--    ausente :: Integral a => [a] -> a
-- tal que (ausente xs) es el único término ausente de la progresión
-- aritmética xs. Por ejemplo,
--    ausente [3,7,9,11]               ==  5
--    ausente [3,5,9,11]               ==  7
--    ausente [3,5,7,11]               ==  9
--    ausente ([1..9]++[11..])         ==  10
--    ausente ([1..10^6] ++ [2+10^6])  ==  1000001
--
-- Nota. Se supone que la lista tiene al menos 3 elementos, que puede
-- ser infinita y que sólo hay un término de la progresión aritmética
-- que no está en la lista. 
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Termino_ausente_en_una_progresion_aritmetica where

import Data.List (group, genericLength)
import Test.QuickCheck (Arbitrary, Gen,
                        arbitrary, frequency, suchThat, quickCheck) 

-- 1ª solución
-- ===========

ausente1 :: Integral a => [a] -> a
ausente1 (x1:xs@(x2:x3:_))
  | d1 == d2     = ausente1 xs
  | d1 == 2 * d2 = x1 + d2
  | d2 == 2 * d1 = x2 + d1
  where d1 = x2 - x1
        d2 = x3 - x2          

-- 2ª solución
-- ===========

ausente2 :: Integral a => [a] -> a
ausente2 s@(x1:x2:x3:_) 
  | x1 + x3 /= 2 * x2 = x1 + (x3 - x2)
  | otherwise         = head [a | (a,b) <- zip [x1,x2..] s
                                , a /= b]

-- 3ª solución
-- ===========

ausente3 :: Integral a => [a] -> a
ausente3  xs@(x1:x2:_) 
  | null us   = x1 + v
  | otherwise = x2 + u * genericLength (u:us) 
  where ((u:us):(v:_):_) = group (zipWith (-) (tail xs) xs)

-- Comprobación de equivalencia
-- ============================

-- Tipo de progresiones aritméticas con un término ausente.
newtype PAconAusente = PA [Integer]
  deriving Show

-- Generación de progresiones aritméticas con un término ausente. 
progresionConAusenteArbitraria :: Gen PAconAusente
progresionConAusenteArbitraria = do
  x <- arbitrary
  d <- arbitrary `suchThat` (/= 0)
  n <- arbitrary `suchThat` (> 2)
  k <- arbitrary `suchThat` (> 0)
  let (as,_:bs) = splitAt k [x,x+d..]
  frequency [(1,return (PA (as ++ bs))),
             (1,return (PA (take (length as + n) (as ++ bs))))]

-- Inclusión del tipo PAconAusente en Arbitrary.
instance Arbitrary PAconAusente where
  arbitrary = progresionConAusenteArbitraria

-- La propiedad es
prop_ausente :: PAconAusente -> Bool 
prop_ausente (PA xs) =
  all (== ausente1 xs)
      [ausente2 xs,
       ausente3 xs]

-- La comprobación es
--    λ> quickCheck prop_ausente
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> let n = 10^6 in ausente1 ([1..n] ++ [n+2])
--    1000001
--    (1.15 secs, 560,529,520 bytes)
--    λ> let n = 10^6 in ausente2 ([1..n] ++ [n+2])
--    1000001
--    (0.33 secs, 336,530,680 bytes)
--    λ> let n = 10^6 in ausente3 ([1..n] ++ [n+2])
--    1000001
--    (0.50 secs, 498,047,584 bytes)
