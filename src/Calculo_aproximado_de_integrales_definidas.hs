-- Calculo_aproximado_de_integrales_definidas.hs
-- Cálculo aproximado de integrales definidas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La integral definida de una función f entre los límites a y b puede
-- calcularse mediante la regla del rectángulo http://bit.ly/1FDhZ1z
-- usando la fórmula  
--    h * (f(a+h/2) + f(a+h+h/2) + f(a+2h+h/2) + ... + f(a+n*h+h/2))
-- con a+n*h+h/2 <= b < a+(n+1)*h+h/2 y usando valores pequeños para h.
-- 
-- Definir la función
--    integral :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
-- tal que (integral a b f h) es el valor de dicha expresión. Por
-- ejemplo, el cálculo de la integral de f(x) = x^3 entre 0 y 1, con
-- paso 0.01, es 
--    integral 0 1 (^3) 0.01  ==  0.24998750000000042
-- Otros ejemplos son
--    integral 0 1 (^4) 0.01                   ==  0.19998333362500048
--    integral 0 1 (\x -> 3*x^2 + 4*x^3) 0.01  ==  1.9999250000000026
--    log 2 - integral 1 2 (\x -> 1/x) 0.01         ==  3.124931644782336e-6
--    pi - 4 * integral 0 1 (\x -> 1/(x^2+1)) 0.01  ==  -8.333333331389525e-6
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Calculo_aproximado_de_integrales_definidas where

import Test.QuickCheck.HigherOrder (quickCheck')
import Test.QuickCheck (Property, (==>), quickCheck)

-- 1ª solución
-- ===========

integral1 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral1 a b f h
  | a+h/2 > b = 0
  | otherwise = h * f (a+h/2) + integral1 (a+h) b f h

-- 2ª solución
-- ===========

integral2 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral2 a b f h = aux a where
  aux x | x+h/2 > b = 0
        | otherwise = h * f (x+h/2) + aux (x+h)

-- 3ª solución
-- ===========

integral3 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral3 a b f h = h * suma (a+h/2) b (+h) f

-- (suma a b s f) es l valor de
--    f(a) + f(s(a)) + f(s(s(a)) + ... + f(s(...(s(a))...))
-- hasta que s(s(...(s(a))...)) > b. Por ejemplo,
--    suma 2 5 (1+) (^3)  ==  224
suma :: (Ord t, Num a) => t -> t -> (t -> t) -> (t -> a) -> a
suma a b s f = sum [f x | x <- sucesion a b s]

-- (sucesion x y s) es la lista
--    [a, s(a), s(s(a), ..., s(...(s(a))...)]
-- hasta que s(s(...(s(a))...)) > b. Por ejemplo,
--    sucesion 3 20 (+2)  ==  [3,5,7,9,11,13,15,17,19]
sucesion :: Ord a => a -> a -> (a -> a) -> [a]
sucesion a b s = takeWhile (<=b) (iterate s a)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_integral :: Int -> Int -> (Int -> Int) -> Int -> Property
prop_integral a b f h =
  a < b && h > 0 ==> 
  all (=~ integral1 a' b' f' h')
      [integral2 a' b' f' h',
       integral3 a' b' f' h']
  where
    a' = fromIntegral a
    b' = fromIntegral b
    h' = fromIntegral h
    f' = fromIntegral . f. round
    x =~ y = abs (x - y) < 0.001
        
-- La comprobación es
--    λ> quickCheck' prop_integral
--    +++ OK, passed 100 tests; 385 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> integral1 0 10 (^3) 0.00001
--    2499.999999881125
--    (2.63 secs, 1,491,006,744 bytes)
--    λ> integral2 0 10 (^3) 0.00001
--    2499.999999881125
--    (1.93 secs, 1,419,006,696 bytes)
--    λ> integral3 0 10 (^3) 0.00001
--    2499.9999998811422
--    (1.28 secs, 817,772,216 bytes)

