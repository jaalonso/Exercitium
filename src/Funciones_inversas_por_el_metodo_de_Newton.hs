-- Funciones_inversas_por_el_metodo_de_Newton.hs
-- Funciones inversas por el método de Newton.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-noviembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir, usando puntoCero, la función
--    inversa :: (Double -> Double) -> Double -> Double
-- tal que (inversa g x) es el valor de la inversa de g en x. Por
-- ejemplo,
--    inversa (^2) 9  ==  3.000000002941184
--
-- Definir, usando inversa, las funciones raizCuadrada, raizCubica,
-- arcoseno y arcocoseno que calculen la raíz cuadrada, la raíz cúbica,
-- el arco seno y el arco coseno, respectivamente. Por ejemplo,
--    raizCuadrada 9  ==  3.000000002941184
--    raizCubica 27   ==  3.0000000000196048
--    arcoseno 1      ==  1.5665489428306574
--    arcocoseno 0    ==  1.5707963267949576
-- ---------------------------------------------------------------------

module Funciones_inversas_por_el_metodo_de_Newton where

import Metodo_de_Newton_para_calcular_raices (puntoCero)
import Test.Hspec (Spec, hspec, it, shouldBe)

inversa :: (Double -> Double) -> Double -> Double
inversa g a = puntoCero f
  where f x = g x - a

raizCuadrada, raizCubica, arcoseno, arcocoseno :: Double -> Double
raizCuadrada = inversa (^2)
raizCubica   = inversa (^3)
arcoseno     = inversa sin
arcocoseno   = inversa cos

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    inversa (^2) 9  `shouldBe`  3.000000002941184
  it "e2" $
    raizCuadrada 9  `shouldBe`  3.000000002941184
  it "e3" $
    raizCubica 27   `shouldBe`  3.0000000000196048
  it "e4" $
    arcoseno 1      `shouldBe`  1.5665489428306574
  it "e5" $
    arcocoseno 0    `shouldBe`  1.5707963267949576

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--
--    Finished in 0.0006 seconds
--    5 examples, 0 failures
