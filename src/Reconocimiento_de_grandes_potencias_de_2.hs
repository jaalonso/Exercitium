-- Reconocimiento_de_grandes_potencias_de_2.hs
-- Reconocimiento de potencias de 2.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 09-Febrero-2015 (actualizado 1-Febrero-2026)
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

import Data.Bits ((.&.), popCount)
import Data.Numbers.Primes (primeFactors)
import Math.NumberTheory.Logarithms (integerLog2)
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

esPotenciaDeDos4 :: Integer -> Bool
esPotenciaDeDos4 n = n == 2 ^ integerLog2 n

-- 5ª solución
-- ===========

-- Usando la función (.&.) de la librería Data.Bits. Dicha función
-- calcula el número correspondiente a la conjunción de las
-- representaciones binarias de sus argumentos. Por ejemplo,
--    6 .&. 3 == 2
-- ya que
--    la representación binaria de 6 es       [1,1,0]
--    la representación binaria de 3 es       [1,1]
--    la conjunción es                        [1,0]
--    la representación decimal de [1,0] es   2
--
-- Otros ejemplos:
--    5 .&. 3 ==   [1,0,0] .&.   [1,1] == 0
--    8 .&. 7 == [1,0,0,0] .&. [1,1,1] = 0
--
-- Usa el truco de que una potencia de 2 en binario tiene exactamente un
-- bit en 1, y al restarle 1, todos los bits se invierten. La operación
-- (.&.) entre ambos da 0. Por ejemplo:
--    8 = 1000₂, 7 = 0111₂ -> 1000 .&. 0111 = 0000
--    6 = 0110₂, 5 = 0101₂ -> 0110 .&. 0101 = 0100

esPotenciaDeDos5 :: Integer -> Bool
esPotenciaDeDos5 n = n .&. (n-1) == 0

-- 6ª solución
-- ===========

esPotenciaDeDos6 :: Integer -> Bool
esPotenciaDeDos6 n = popCount n == 1

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
  describe "def. 5" $ specG esPotenciaDeDos5
  describe "def. 6" $ specG esPotenciaDeDos6

-- La verificación es
--    λ> verifica
--    36 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_esPotenciaDeDos :: Positive Integer -> Bool
prop_esPotenciaDeDos (Positive n) =
  all (== esPotenciaDeDos1 n)
      [ esPotenciaDeDos2 n
      , esPotenciaDeDos3 n
      , esPotenciaDeDos4 n
      , esPotenciaDeDos5 n
      , esPotenciaDeDos6 n
      ]

-- La comprobación es
--    λ> quickCheck prop_esPotenciaDeDos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esPotenciaDeDos1 (2^(3*10^5))
--    True
--    (3.07 secs, 5,713,366,248 bytes)
--    λ> esPotenciaDeDos2 (2^(3*10^5))
--    True
--    (5.86 secs, 5,665,328,552 bytes)
--    λ> esPotenciaDeDos3 (2^(3*10^5))
--    True
--    (2.39 secs, 5,749,363,752 bytes)
--    λ> esPotenciaDeDos4 (2^(3*10^5))
--    True
--    (0.02 secs, 844,400 bytes)
--    λ> esPotenciaDeDos5 (2^(3*10^5))
--    True
--    (0.03 secs, 803,456 bytes)
--    λ> esPotenciaDeDos6 (2^(3*10^5))
--    True
--    (0.03 secs, 728,296 bytes)
--
--    λ> esPotenciaDeDos4 (2^(2*10^8))
--    True
--    (3.78 secs, 148,890,576 bytes)
--    λ> esPotenciaDeDos5 (2^(2*10^8))
--    True
--    (1.83 secs, 124,751,352 bytes)
--    λ> esPotenciaDeDos6 (2^(2*10^8))
--    True
--    (1.83 secs, 74,751,256 bytes)
