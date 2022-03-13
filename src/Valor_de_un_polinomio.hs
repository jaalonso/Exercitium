-- Valor_de_un_polinomio.hs
-- Valores de polinomios representados con vectores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los polinomios se pueden representar mediante vectores usando la
-- librería Data.Array. En primer lugar, se define el tipo de los
-- polinomios (con coeficientes de tipo a) mediante
--    type Polinomio a = Array Int a
-- Como ejemplos, definimos el polinomio
--    ej_pol1 :: Array Int Int
--    ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]
-- que representa a 2x - 5x^2 + 7x^4 + 6 y el polinomio
--    ej_pol2 :: Array Int Double
--    ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]
-- que representa a 2x - 5.2x^2 + 7x^4 + 6.5
--
-- Definir la función
--    valor :: Num a => Polinomio a -> a -> a
-- tal que (valor p b) es el valor del polinomio p en el punto b. Por
-- ejemplo,
--    valor ej_pol1 0  ==  6
--    valor ej_pol1 1  ==  10
--    valor ej_pol1 2  ==  102
--    valor ej_pol2 0  ==  6.5
--    valor ej_pol2 1  ==  10.3
--    valor ej_pol2 3  ==  532.7
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Valor_de_un_polinomio where

import Data.Array (Array, (!), array, assocs, bounds, elems, listArray)
import Test.QuickCheck

type Polinomio a = Array Int a

ej_pol1 :: Array Int Int
ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]

ej_pol2 :: Array Int Double
ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]

-- 1ª solución
valor1 :: Num a => Polinomio a -> a -> a
valor1 p b = sum [(p!i)*b^i | i <- [0..n]]
  where (_,n) = bounds p

-- 2ª solución
valor2 :: Num a => Polinomio a -> a -> a
valor2 p b = sum [(p!i)*b^i | i <- [0..length p - 1]]

-- 3ª solución
valor3 :: Num a => Polinomio a -> a -> a
valor3 p b = sum [v*b^i | (i,v) <- assocs p]

-- 4ª solución
valor4 :: Num a => Polinomio a -> a -> a
valor4 p b = foldl (\c r -> c*b + r) 0 (reverse (elems p))

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_valor :: [Integer] -> Integer -> Bool
prop_valor xs b =
  all (== valor1 p b)
      [valor2 p b,
       valor3 p b,
       valor4 p b]
  where p = listArray (0, length xs - 1) xs

-- La comprobación es
--    λ> quickCheck prop_valor
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (valor1 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (7.84 secs, 2,953,933,368 bytes)
--    λ> length (show (valor2 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (7.93 secs, 2,953,933,264 bytes)
--    λ> length (show (valor3 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (7.87 secs, 2,954,733,184 bytes)
--    λ> length (show (valor4 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.48 secs, 1,298,372,232 bytes)
