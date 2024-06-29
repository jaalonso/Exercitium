-- Reconocimiento_de_grandes_potencias_de_2.hs
-- Reconocimiento de potencias de 2.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-junio-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    esPotenciaDeDos :: Integer -> Bool
-- tal que (esPotenciaDeDos n) se verifica si n es una potencia de
-- dos (suponiendo que n es mayor que 0). Por ejemplo.
--    esPotenciaDeDos    1        == True
--    esPotenciaDeDos    2        == True
--    esPotenciaDeDos    6        == False
--    esPotenciaDeDos    8        == True
--    esPotenciaDeDos 1024        == True
--    esPotenciaDeDos 1026        == False
--    esPotenciaDeDos (2^(10^8))  == True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Reconocimiento_de_grandes_potencias_de_2 where

import Data.Bits ((.&.))
import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

esPotenciaDeDos1 :: Integer -> Bool
esPotenciaDeDos1 1 = True
esPotenciaDeDos1 n
  | even n    = esPotenciaDeDos1 (n `div` 2)
  | otherwise = False

-- 2ª solución
-- ===========

esPotenciaDeDos2 :: Integer -> Bool
esPotenciaDeDos2 n = n ==
  head (dropWhile (<n) potenciasDeDos)

-- potenciasDeDos es la lista de las potencias de dos. Por ejemplo,
--    take 10 potenciasDeDos  == [1,2,4,8,16,32,64,128,256,512]
potenciasDeDos :: [Integer]
potenciasDeDos = iterate (*2) 1

-- 3ª solución
-- ===========

esPotenciaDeDos3 :: Integer -> Bool
esPotenciaDeDos3 x = all (==2) (primeFactors x)

-- 4ª solución
-- ===========

-- Usando la función (.&.) de la librería Data.Bits. Dicha función
-- calcula el número correspondiente a la conjunción de las
-- representaciones binarias de sus argumentos. Por ejemplo,
--    6 .&. 3 == 2
-- ya que
--    la representación binaria de 6 es     [1,1,0]
--    la representación binaria de 3 es       [1,1]
--    la conjunción es                        [1,0]
--    la representación decimal de [1,0] es   2
--
-- Otros ejemplos:
--    4 .&. 3 ==   [1,0,0] .&.   [1,1] == 0
--    8 .&. 7 == [1,0,0,0] .&. [1,1,1] = 0

esPotenciaDeDos4 :: Integer -> Bool
esPotenciaDeDos4 n = n .&. (n-1) == 0

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Bool) -> Spec
specG esPotenciaDeDos = do
  it "e1" $
    esPotenciaDeDos    1 `shouldBe` True
  it "e2" $
    esPotenciaDeDos    2 `shouldBe` True
  it "e3" $
    esPotenciaDeDos    6 `shouldBe` False
  it "e4" $
    esPotenciaDeDos    8 `shouldBe` True
  it "e5" $
    esPotenciaDeDos 1024 `shouldBe` True
  it "e6" $
    esPotenciaDeDos 1026 `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG esPotenciaDeDos1
  describe "def. 2" $ specG esPotenciaDeDos2
  describe "def. 3" $ specG esPotenciaDeDos3
  describe "def. 4" $ specG esPotenciaDeDos4

-- La verificación es
--    λ> verifica
--    24 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_esPotenciaDeDos :: Positive Integer -> Bool
prop_esPotenciaDeDos (Positive n) =
  all (== esPotenciaDeDos1 n)
      [ esPotenciaDeDos2 n
      , esPotenciaDeDos3 n
      , esPotenciaDeDos4 n
      ]

-- La comprobación es
--    λ> quickCheck prop_esPotenciaDeDos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esPotenciaDeDos1 (2^(3*10^5))
--    True
--    (3.51 secs, 5,730,072,544 bytes)
--    λ> esPotenciaDeDos2 (2^(3*10^5))
--    True
--    (3.12 secs, 5,755,639,952 bytes)
--    λ> esPotenciaDeDos3 (2^(3*10^5))
--    True
--    (2.92 secs, 5,758,872,040 bytes)
--    λ> esPotenciaDeDos4 (2^(3*10^5))
--    True
--    (0.03 secs, 715,152 bytes)
