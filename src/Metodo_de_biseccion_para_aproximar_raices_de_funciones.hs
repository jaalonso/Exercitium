-- Metodo_de_biseccion_para_aproximar_raices_de_funciones.hs
-- Método de bisección para aproximar raíces de funciones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El método de bisección para calcular un cero de una función en el
-- intervalo [a,b] se basa en el teorema de Bolzano: 
--    "Si f(x) es una función continua en el intervalo [a, b], y si,
--    además, en los extremos del intervalo la función f(x) toma valores
--    de signo opuesto (f(a) * f(b) < 0), entonces existe al menos un
--    valor c en (a, b) para el que f(c) = 0".
--
-- El método para calcular un cero de la función f en el intervalo [a,b]
-- con un error menor que e consiste en tomar el punto medio del
-- intervalo c = (a+b)/2 y considerar los siguientes casos:
-- (*) Si |f(c)| < e, hemos encontrado una aproximación del punto que
--     anula f en el intervalo con un error aceptable.
-- (*) Si f(c) tiene signo distinto de f(a), repetir el proceso en el
--     intervalo [a,c].
-- (*) Si no, repetir el proceso en el intervalo [c,b].
-- 
-- Definir la función
--    biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double
-- tal que (biseccion f a b e) es una aproximación del punto del
-- intervalo [a,b] en el que se anula la función f, con un error menor
-- que e, calculada mediante el método de la bisección. Por ejemplo,
--    biseccion (\x -> x^2 - 3) 0 5 0.01             ==  1.7333984375
--    biseccion (\x -> x^3 - x - 2) 0 4 0.01         ==  1.521484375
--    biseccion cos 0 2 0.01                         ==  1.5625
--    biseccion (\x -> log (50-x) - 4) (-10) 3 0.01  ==  -5.125
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Metodo_de_biseccion_para_aproximar_raices_de_funciones where

import Test.QuickCheck (Arbitrary, Gen, Property, (==>), arbitrary, choose, sized, quickCheck)

-- 1ª solución
-- ===========

biseccion1 :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion1 f a b e  
    | abs (f c) < e = c
    | f a * f c < 0 = biseccion1 f a c e
    | otherwise     = biseccion1 f c b e
    where c = (a+b)/2

-- 2ª solución
-- ===========

biseccion2 :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion2 f a b e = aux a b
  where aux a' b' | abs (f c) < e = c
                  | f a' * f c < 0 = aux a' c 
                  | otherwise     = aux c b'
          where c = (a'+b')/2

-- Comprobación de equivalencia
-- ============================

newtype Polinomio = P [Int]
  deriving Show

valorPolinomio :: Polinomio -> Double -> Double
valorPolinomio (P cs) x =
  sum [fromIntegral c * x^n | (c,n) <- zip cs [0..]]

polinomioArbitrario :: Int -> Gen Polinomio
polinomioArbitrario 0 = return (P [])
polinomioArbitrario n = do
  c <- choose (-10,10)
  (P xs) <- polinomioArbitrario (n `div` 2)
  return (P (c:xs))

instance Arbitrary Polinomio where
  arbitrary = sized polinomioArbitrario

-- La propiedad es
prop_biseccion :: Polinomio -> Double -> Double -> Double -> Property
prop_biseccion p a b e =
  f a * f b < 0 && e > 0 ==>
  biseccion1 f a b e =~ biseccion2 f a b e
  where
    f = valorPolinomio p 
    x =~ y = abs (x - y) < 0.001

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxDiscardRatio=30}) prop_biseccion
--    +++ OK, passed 100 tests; 2156 discarded.
