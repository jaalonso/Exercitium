-- Reconocimiento_de_potencias_de_4.hs
-- Reconocimiento de potencias de 4.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 04-febrero-2022 y 14-marzo-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    esPotenciaDe4 :: Integral a => a -> Bool
-- tal que (esPotenciaDe4 n) se verifica si n es una potencia de 4. Por
-- ejemplo,
--    esPotenciaDe4 16                ==  True
--    esPotenciaDe4 17                ==  False
--    esPotenciaDe4 (4^(4*10^5))      ==  True
--    esPotenciaDe4 (1 + 4^(4*10^5))  ==  False
-- ---------------------------------------------------------------------

module Reconocimiento_de_potencias_de_4 where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

esPotenciaDe4_1 :: Integral a => a -> Bool
esPotenciaDe4_1 0 = False
esPotenciaDe4_1 1 = True
esPotenciaDe4_1 n = n `mod` 4 == 0 && esPotenciaDe4_1 (n `div` 4)

-- 2ª solución
-- ===========

esPotenciaDe4_2 :: Integral a => a -> Bool
esPotenciaDe4_2 n = n `pertenece` potenciasDe4

-- potenciassDe4 es la lista de las potencias de 4. Por ejemplo,
--    take 5 potenciasDe4  ==  [1,4,16,64,256]
potenciasDe4 :: Integral a => [a]
potenciasDe4 = [4^x | x <- [0..]]

-- (pertenece x ys) se verifica si x pertenece a la lista ordenada
-- (posiblemente infinita xs). Por ejemplo,
--    pertenece 8 [2,4..]  ==  True
--    pertenece 9 [2,4..]  ==  False
pertenece :: Integral a => a -> [a] -> Bool
pertenece x ys = x == head (dropWhile (<x) ys)

-- 3ª solución
-- ===========

esPotenciaDe4_3 :: Integral a => a -> Bool
esPotenciaDe4_3 n = n `pertenece` potenciasDe4_2

-- potenciassDe4 es la lista de las potencias de 4. Por ejemplo,
--    take 5 potenciasDe4  ==  [1,4,16,64,256]
potenciasDe4_2 :: Integral a => [a]
potenciasDe4_2 = iterate (*4) 1

-- 4ª solución
-- ===========

esPotenciaDe4_4 :: Integral n => n -> Bool
esPotenciaDe4_4 n =
  n == head (dropWhile (<n) (iterate (*4) 1))

-- 5ª solución
-- ===========

esPotenciaDe4_5 :: Integral n => n -> Bool
esPotenciaDe4_5 n =
  n == until (>=n) (*4) 1

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Bool) -> Spec
specG esPotenciaDe4 = do
  it "e1" $
    esPotenciaDe4 16 `shouldBe` True
  it "e2" $
    esPotenciaDe4 17 `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG esPotenciaDe4_1
  describe "def. 2" $ specG esPotenciaDe4_2
  describe "def. 3" $ specG esPotenciaDe4_3
  describe "def. 4" $ specG esPotenciaDe4_4
  describe "def. 5" $ specG esPotenciaDe4_5

-- La verificación es
--    λ> verifica
--
--    10 examples, 0 failures

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esPotenciaDe4_1 (4^(4*10^4))
--    True
--    (0.18 secs, 233,903,248 bytes)
--    λ> esPotenciaDe4_2 (4^(4*10^4))
--    True
--    (2.01 secs, 756,125,712 bytes)
--    λ> esPotenciaDe4_3 (4^(4*10^4))
--    True
--    (0.05 secs, 212,019,464 bytes)
--    λ> esPotenciaDe4_4 (4^(4*10^4))
--    True
--    (0.05 secs, 212,019,368 bytes)
--    λ> esPotenciaDe4_5 (4^(4*10^4))
--    True
--    (0.07 secs, 209,779,888 bytes)
--
--    λ> esPotenciaDe4_3 (4^(2*10^5))
--    True
--    (0.64 secs, 5,184,667,280 bytes)
--    λ> esPotenciaDe4_4 (4^(2*10^5))
--    True
--    (0.64 secs, 5,184,667,200 bytes)
--    λ> esPotenciaDe4_5 (4^(2*10^5))
--    True
--    (0.63 secs, 5,173,467,656 bytes)
--
--    λ> esPotenciaDe4_3 (4^(4*10^5))
--    True
--    (2.27 secs, 20,681,727,464 bytes)
--    λ> esPotenciaDe4_4 (4^(4*10^5))
--    True
--    (2.30 secs, 20,681,727,320 bytes)
--    λ> esPotenciaDe4_5 (4^(4*10^5))
--    True
--    (2.28 secs, 20,659,327,352 bytes)
