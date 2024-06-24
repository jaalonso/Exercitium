-- Numeros_con_digitos_primos.hs
-- Números con todos sus dígitos primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-junio-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la lista
--    numerosConDigitosPrimos :: [Integer]
-- cuyos elementos son los números con todos sus dígitos primos. Por
-- ejemplo,
--    λ> take 22 numerosConDigitosPrimos
--    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]
--    λ> numerosConDigitosPrimos !! (10^7)
--    322732232572
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_con_digitos_primos where

import Test.QuickCheck (NonNegative (NonNegative), quickCheck)
import Data.Char (intToDigit)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

numerosConDigitosPrimos1 :: [Integer]
numerosConDigitosPrimos1 = [n | n <- [2..], digitosPrimos n]

-- (digitosPrimos n) se verifica si todos los dígitos de n son
-- primos. Por ejemplo,
--    digitosPrimos 352  ==  True
--    digitosPrimos 362  ==  False
digitosPrimos :: Integer -> Bool
digitosPrimos n = subconjunto (digitos n) [2,3,5,7]

-- (digitos n) es la lista de las digitos de n. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo,
--    subconjunto [3,2,5,2] [2,7,3,5]  ==  True
--    subconjunto [3,2,5,2] [2,7,2,5]  ==  False
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [x `elem` ys | x <- xs]

-- 2ª solución
-- ===========

numerosConDigitosPrimos2 :: [Integer]
numerosConDigitosPrimos2 =
  filter (all (`elem` "2357") . show) [2..]

-- 3ª solución
-- ===========

--    λ> take 60 numerosConDigitosPrimos2
--    [  2,  3,  5,  7,
--      22, 23, 25, 27,
--      32, 33, 35, 37,
--      52, 53, 55, 57,
--      72, 73, 75, 77,
--     222,223,225,227,
--     232,233,235,237,
--     252,253,255,257,
--     272,273,275,277,
--     322,323,325,327,
--     332,333,335,337,
--     352,353,355,357,
--     372,373,375,377,
--     522,523,525,527,
--     532,533,535,537]

numerosConDigitosPrimos3 :: [Integer]
numerosConDigitosPrimos3 =
  [2,3,5,7] ++ [10*n+d | n <- numerosConDigitosPrimos3, d <- [2,3,5,7]]

-- 4ª solución
-- ===========

--    λ> take 60 numerosConDigitosPrimos2
--    [ 2, 3, 5, 7,
--     22,23,25,27,
--     32,33,35,37,
--     52,53,55,57,
--     72,73,75,77,
--     222,223,225,227, 232,233,235,237, 252,253,255,257, 272,273,275,277,
--     322,323,325,327, 332,333,335,337, 352,353,355,357, 372,373,375,377,
--     522,523,525,527, 532,533,535,537]

numerosConDigitosPrimos4 :: [Integer]
numerosConDigitosPrimos4 = concat (iterate siguiente [2,3,5,7])

-- (siguiente xs) es la lista obtenida añadiendo delante de cada
-- elemento de xs los dígitos 2, 3, 5 y 7. Por ejemplo,
--    λ> siguiente [5,6,8]
--    [25,26,28,
--     35,36,38,
--     55,56,58,
--     75,76,78]
siguiente :: [Integer] -> [Integer]
siguiente xs = concat [map (pega d) xs | d <- [2,3,5,7]]

-- (pega d n) es el número obtenido añadiendo el dígito d delante del
-- número n. Por ejemplo,
--    pega 3 35  ==  335
pega :: Int -> Integer -> Integer
pega d n = read (intToDigit d : show n)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG numerosConDigitosPrimos = do
  it "e1" $
    take 22 numerosConDigitosPrimos `shouldBe`
    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]

spec :: Spec
spec = do
  describe "def. 1" $ specG numerosConDigitosPrimos1
  describe "def. 2" $ specG numerosConDigitosPrimos2
  describe "def. 3" $ specG numerosConDigitosPrimos3
  describe "def. 4" $ specG numerosConDigitosPrimos4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numerosConDigitosPrimos_equiv :: NonNegative Int -> Bool
prop_numerosConDigitosPrimos_equiv (NonNegative n) =
  all (== numerosConDigitosPrimos1 !! n)
      [ numerosConDigitosPrimos2 !! n
      , numerosConDigitosPrimos3 !! n
      , numerosConDigitosPrimos4 !! n
      ]

-- La comprobación es
--    λ> quickCheck prop_numerosConDigitosPrimos_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numerosConDigitosPrimos1 !! 5000
--    752732
--    (2.45 secs, 6,066,926,272 bytes)
--    λ> numerosConDigitosPrimos2 !! 5000
--    752732
--    (0.34 secs, 387,603,456 bytes)
--    λ> numerosConDigitosPrimos3 !! 5000
--    752732
--    (0.01 secs, 1,437,624 bytes)
--    λ> numerosConDigitosPrimos4 !! 5000
--    752732
--    (0.00 secs, 1,556,104 bytes)
--
--    λ> numerosConDigitosPrimos3 !! (10^7)
--    322732232572
--    (3.94 secs, 1,820,533,328 bytes)
--    λ> numerosConDigitosPrimos4 !! (10^7)
--    322732232572
--    (1.84 secs, 2,000,606,640 bytes)
