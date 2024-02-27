-- Suma_de_fila_del_triangulo_de_los_impares.hs
-- Suma fila del triángulo de los impares
-- José A. Alonso Jiménez
-- Sevilla, 27 de febrero 2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se condidera el siguiente triángulo de números impares
--
--              1
--           3     5
--        7     9    11
--    13    15    17    19
-- 21    23    25    27    29
-- ...
--
-- Definir la función
--    sumaFilaTrianguloImpares :: Integer -> Integer
-- tal que (sumaFilaTrianguloImpares n) es la suma de la n-ésima fila
-- del triángulo de los números impares. Por ejemplo,
--    sumaFilaTrianguloImpares 1  ==  1
--    sumaFilaTrianguloImpares 2  ==  8
--    length (show (sumaFilaTrianguloImpares (10^500)))    ==  1501
--    length (show (sumaFilaTrianguloImpares (10^5000)))   ==  15001
--    length (show (sumaFilaTrianguloImpares (10^50000)))  ==  150001
-- ---------------------------------------------------------------------

module Suma_de_fila_del_triangulo_de_los_impares where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
sumaFilaTrianguloImpares1 :: Integer -> Integer
sumaFilaTrianguloImpares1 n =
  sum [n^2-n+1, n^2-n+3 .. n^2+n-1]

-- 2ª solución
sumaFilaTrianguloImpares2 :: Integer -> Integer
sumaFilaTrianguloImpares2 = (^3)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaFilaTrianguloImpares = do
  it "e1" $
    sumaFilaTrianguloImpares 1  `shouldBe`  1
  it "e2" $
    sumaFilaTrianguloImpares 2  `shouldBe`  8

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaFilaTrianguloImpares1
  describe "def. 2" $ specG sumaFilaTrianguloImpares2

-- La verificación es
--    λ> verifica
--
--    4 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_sumaFilaTrianguloImpares :: Positive Integer -> Bool
prop_sumaFilaTrianguloImpares (Positive n) =
  sumaFilaTrianguloImpares1 n == sumaFilaTrianguloImpares2 n

-- La comprobación es
--    λ> quickCheck prop_sumaFilaTrianguloImpares
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (sumaFilaTrianguloImpares1 (10^7)))
--    22
--    (2.91 secs, 2,167,239,232 bytes)
--    λ> length (show (sumaFilaTrianguloImpares2 (10^7)))
--    22
--    (0.01 secs, 102,584 bytes)
