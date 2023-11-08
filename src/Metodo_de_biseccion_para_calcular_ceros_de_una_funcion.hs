-- Metodo_de_biseccion_para_calcular_ceros_de_una_funcion.hs
-- Método de bisección para calcular ceros de una función.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-noviembre-2023
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
-- + Si |f(c)| < e, hemos encontrado una aproximación del punto que
--   anula f en el intervalo con un error aceptable.
-- + Si f(c) tiene signo distinto de f(a), repetir el proceso en el
--   intervalo [a,c].
-- + Si no, repetir el proceso en el intervalo [c,b].
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

module Metodo_de_biseccion_para_calcular_ceros_de_una_funcion where

import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª solución
-- ===========

biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion f a b e
  | abs (f c) < e = c
  | f a * f c < 0 = biseccion f a c e
  | otherwise     = biseccion f c b e
  where c = (a+b)/2

-- 2ª solución
-- ===========

biseccion2 :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion2 f a b e = aux a b
  where aux a' b' | abs (f c) < e   = c
                  | f a' * f c < 0  = aux a' c
                  | otherwise       = aux c b'
          where c = (a'+b')/2

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    biseccion (\x -> x^2 - 3) 0 5 0.01             `shouldBe`  1.7333984375
  it "e2" $
    biseccion (\x -> x^3 - x - 2) 0 4 0.01         `shouldBe`  1.521484375
  it "e3" $
    biseccion cos 0 2 0.01                         `shouldBe`  1.5625
  it "e4" $
    biseccion (\x -> log (50-x) - 4) (-10) 3 0.01  `shouldBe`  -5.125
  it "e5" $
    biseccion2 (\x -> x^2 - 3) 0 5 0.01             `shouldBe`  1.7333984375
  it "e6" $
    biseccion2 (\x -> x^3 - x - 2) 0 4 0.01         `shouldBe`  1.521484375
  it "e7" $
    biseccion2 cos 0 2 0.01                         `shouldBe`  1.5625
  it "e8" $
    biseccion2 (\x -> log (50-x) - 4) (-10) 3 0.01  `shouldBe`  -5.125

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--    e7
--    e8
--
--    Finished in 0.0008 seconds
--    8 examples, 0 failures
