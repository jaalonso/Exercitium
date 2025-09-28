-- Sucesiones_pucelanas.hs
-- Sucesiones pucelanas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-Julio-2014 (actualizado 28-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En la Olimpiada de Matemática del 2010 se planteó el siguiente
-- problema:
--   Una sucesión pucelana es una sucesión creciente de 16 números
--   impares positivos consecutivos, cuya suma es un cubo perfecto.
--   ¿Cuántas sucesiones pucelanas tienen solamente números de tres
--   cifras?
-- Para resolverlo se propone el siguiente ejercicio.
--
-- Definir la función
--    pucelanasConNcifras :: Integer -> [[Integer]]
-- tal que (pucelanasConNcifras n) es la lista de las sucesiones
-- pucelanas que tienen solamente números de n cifras. Por ejemplo,
--    λ> pucelanasConNcifras 2
--    [[17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47]]
--
-- Calcular cuántas sucesiones pucelanas tienen solamente números de
-- tres cifras.
-- ---------------------------------------------------------------------

module Sucesiones_pucelanas where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

pucelanasConNcifras1 :: Integer -> [[Integer]]
pucelanasConNcifras1 n =
  [[x,x+2..x+30] | x <- [10^(n-1)+1..10^n-31],
                   esCubo (sum [x,x+2..x+30])]

-- (esCubo n) se verifica si n es un cubo. Por ejemplo,
--    esCubo 27  ==  True
--    esCubo 28  ==  False
esCubo :: Integer -> Bool
esCubo x = y^3 == x
  where y = ceiling (fromIntegral x ** (1/3))

-- 2ª solución
-- ===========

pucelanasConNcifras2 :: Integer -> [[Integer]]
pucelanasConNcifras2 n =
  [[x,x+2..x+30] | x <- [10^(n-1)+1..10^n-31],
                   esCubo (16 * fromIntegral x + 240)]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [[Integer]]) -> Spec
specG pucelanasConNcifras = do
  it "e1" $
    pucelanasConNcifras 2 `shouldBe`
    [[17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47]]

spec :: Spec
spec = do
  describe "def. 1" $ specG pucelanasConNcifras1
  describe "def. 2" $ specG pucelanasConNcifras2

-- La verificación es
--    λ> verifica
--    3 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_pucelanasConNcifras :: Integer -> Bool
prop_pucelanasConNcifras m =
  and [pucelanasConNcifras1 n == pucelanasConNcifras2 n
      | n <- [1..m]]

-- La comprobación es
--    λ> prop_pucelanasConNcifras 6
--    True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> head (head (pucelanasConNcifras1 10))
--    1000187985
--    (0.51 secs, 590,126,640 bytes)
--    λ> head (head (pucelanasConNcifras2 10))
--    1000187985
--    (0.39 secs, 286,342,880 bytes)

-- Cálculo
-- =======

-- El cálculo es
--    λ> length (pucelanasConNcifras1 3)
--    3
