-- Metodo_de_Newton_para_calcular_raices.hs
-- Método de Newton para calcular raíces.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-noviembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los ceros de una función pueden calcularse mediante el método de
-- Newton basándose en las siguientes propiedades:
-- + Si b es una aproximación para el punto cero de f, entonces
--   b-f(b)/f'(b) es una mejor aproximación.
-- + el límite de la sucesión x_n definida por
--      x_0     = 1
--      x_{n+1} = x_n-f(x_n)/f'(x_n)
--   es un cero de f.
--
-- Definir la función
--    puntoCero :: (Double -> Double) -> Double
-- tal que (puntoCero f) es un cero de la función f calculado usando la
-- propiedad anterior. Por ejemplo,
--    puntoCero cos  ==  1.5707963267949576
-- ---------------------------------------------------------------------

module Metodo_de_Newton_para_calcular_raices where

import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª solución
-- ===========

puntoCero :: (Double -> Double) -> Double
puntoCero f = puntoCeroAux f 1
  where puntoCeroAux f' x | aceptable x = x
                          | otherwise   = puntoCeroAux f' (mejora x)
        aceptable b = abs (f b) < 0.00001
        mejora b    = b - f b / derivada f b

-- (derivada f x) es el valor de la derivada de la función f en el punto
-- x con aproximación 0.0001. Por ejemplo,
--    derivada sin pi == -0.9999999983354435
--    derivada cos pi == 4.999999969612645e-5
derivada :: (Double -> Double) -> Double -> Double
derivada f x = (f (x+a) - f x)/a
  where a = 0.0001

-- 2ª solución
-- ===========

puntoCero2 :: (Double -> Double) -> Double
puntoCero2 f = until aceptable mejora 1
  where aceptable b = abs (f b) < 0.00001
        mejora b    = b - f b / derivada f b

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    puntoCero cos `shouldBe` 1.5707963267949576
  it "e2" $
    puntoCero2 cos `shouldBe` 1.5707963267949576

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0002 seconds
--    2 examples, 0 failures
