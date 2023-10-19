-- Metodo_de_Heron_para_calcular_la_raiz_cuadrada.hs
-- Método de Herón para calcular la raíz_cuadrada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-octubre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El método de Herón para calcular la raíz cuadrada de un número se
-- basa en las siguientes propiedades:
-- + Si y es una aproximación de la raíz cuadrada de x, entonces
--   (y+x/y)/2 es una aproximación mejor.
-- + El límite de la sucesión definida por
--       x_0     = 1
--       x_{n+1} = (x_n+x/x_n)/2
--   es la raíz cuadrada de x.
--
-- Definir la función
--    raiz :: Double -> Double
-- tal que (raiz x) es la raíz cuadrada de x calculada usando la
-- propiedad anterior con una aproximación de 0.00001 y tomando como
-- valor inicial 1. Por ejemplo,
--    raiz 9  ==  3.000000001396984
-- ---------------------------------------------------------------------

module Metodo_de_Heron_para_calcular_la_raiz_cuadrada where

import Test.QuickCheck
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª solución
-- ===========

raiz :: Double -> Double
raiz x = raizAux 1
  where raizAux y | aceptable y = y
                  | otherwise   = raizAux (mejora y)
        aceptable y = abs(y*y-x) < 0.00001
        mejora y    = 0.5*(y+x/y)

-- 2ª solución
-- ===========

raiz2 :: Double -> Double
raiz2 x = until aceptable mejora 1
  where aceptable y = abs(y*y-x) < 0.00001
        mejora y    = 0.5*(y+x/y)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_raiz :: Positive Double -> Bool
prop_raiz (Positive x) =
  raiz x ~= sqrt x &&
  raiz2 x ~= sqrt x
  where
    a ~= b = abs (a-b) < 0.001

-- La comprobación es
--    λ> quickCheck prop_raiz
--    +++ OK, passed 100 tests.

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    raiz 9 `shouldBe`  3.000000001396984
  it "e2" $
    raiz2 9 `shouldBe`  3.000000001396984

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0008 seconds
--    2 examples, 0 failures
