-- Limites_de_sucesiones.hs
-- Límites de sucesiones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-Julio-2014 (actualizado 7-Septiembre-2025)
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

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

limite1 :: (Double -> Double) -> Double -> Double
limite1 f a = buscaX 1
  where
    buscaX x
      | cumpleCondicion x = f x
      | otherwise         = buscaX (x + 1)
    cumpleCondicion x = all (\y -> abs (f y - f x) < a) [x+1 .. x+100]

-- 2ª solución
-- ===========

limite2 :: (Double -> Double) -> Double -> Double
limite2 f a =
  head [f x | x <- [1..],
              all (\y -> abs (f y - f x) < a) [x+1 .. x+100]]

-- 3ª solución
-- ===========

limite3 :: (Double -> Double) -> Double -> Double
limite3 f a =
  head [f x | x <- [1..],
              maximum [abs (f y - f x) | y <- [x+1..x+100]] < a]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Double -> Double) -> Double -> Double) -> Spec
specG limite = do
  it "e1" $
    limite (\n -> (2*n+1)/(n+5)) 0.001  `shouldBe`  1.9900110987791344
  it "e2" $
    limite (\n -> (1+1/n)**n) 0.001     `shouldBe`  2.714072874546881

spec :: Spec
spec = do
  describe "def. 1"  $ specG limite1
  describe "def. 2"  $ specG limite2
  describe "def. 3"  $ specG limite3

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- Definimos un tipo para representar funciones simples de forma que
-- (F a b c d) representa λn -> (a*n + b)/(c*n + d)
data Funcion = F Int Int Int Int
  deriving Show

-- Para generar funciones arbitrarias
instance Arbitrary Funcion where
  arbitrary = do
    a <- choose (1, 10)
    b <- choose (0, 10)
    c <- choose (1, 10)
    d <- choose (1, 10)
    return (F a b c d)

-- (aFuncion (F a b c d)) es la función λn -> (a*n + b)/(c*n + d)
aFuncion :: Funcion -> Double -> Double
aFuncion (F a b c d) n =
  fromIntegral (a * round n + b) / fromIntegral (c * round n + d)

-- La propiedad es
prop_limite :: Funcion -> Positive Double -> Bool
prop_limite func (Positive a) =
  let f = aFuncion func
      a' = min a 0.1
      l1 = limite1 f a'
      l2 = limite2 f a'
      l3 = limite3 f a'
  in abs (l1 - l2) < 1e-10 && abs (l2 - l3) < 1e-10

-- La comprobación es
--    λ> quickCheck prop_limite
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> limite1 (\n -> (2*n+1)/(n+5)) 0.00001
--    1.9990463070891173
--    (0.52 secs, 298,641,448 bytes)
--    λ> limite2 (\n -> (2*n+1)/(n+5)) 0.00001
--    1.9990463070891173
--    (0.52 secs, 298,264,360 bytes)
--    λ> limite3 (\n -> (2*n+1)/(n+5)) 0.00001
--    1.9990463070891173
--    (1.51 secs, 859,004,120 bytes)
