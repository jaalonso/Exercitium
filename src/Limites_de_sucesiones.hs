-- Limites_de_sucesiones.hs
-- Límites de sucesiones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-noviembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    limite :: (Double -> Double) -> Double -> Double
-- tal que (limite f a) es el valor de f en el primer término x tal que,
-- para todo y entre x+1 y x+100, el valor absoluto de la diferencia
-- entre f(y) y f(x) es menor que a. Por ejemplo,
--    limite (\n -> (2*n+1)/(n+5)) 0.001  ==  1.9900110987791344
--    limite (\n -> (1+1/n)**n) 0.001     ==  2.714072874546881
-- ---------------------------------------------------------------------

module Limites_de_sucesiones where

import Test.Hspec (Spec, hspec, it, shouldBe)

limite :: (Double -> Double) -> Double -> Double
limite f a =
  head [f x | x <- [1..],
              maximum [abs (f y - f x) | y <- [x+1..x+100]] < a]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    limite (\n -> (2*n+1)/(n+5)) 0.001  `shouldBe`  1.9900110987791344
  it "e2" $
    limite (\n -> (1+1/n)**n) 0.001     `shouldBe`  2.714072874546881

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.1927 seconds
--    2 examples, 0 failures
