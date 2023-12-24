-- Numeros_primos_de_Hilbert.hs
-- Números primos de Hilbert.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-diciembre-23
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un [número de Hilbert](http://bit.ly/204SW1p) es un entero positivo
-- de la forma 4n+1. Los primeros números de Hilbert son 1, 5, 9, 13,
-- 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81,
-- 85, 89, 93, 97, ...
--
-- Un primo de Hilbert es un número de Hilbert n que no es divisible
-- por ningún número de Hilbert menor que n (salvo el 1). Los primeros
-- primos de Hilbert son 5, 9, 13, 17, 21, 29, 33, 37, 41, 49, 53, 57,
-- 61, 69, 73, 77, 89, 93, 97, 101, 109, 113, 121, 129, 133, 137, 141,
-- 149, 157, 161, 173, 177, 181, 193, 197, ...
--
-- Definir la sucesión
--    primosH :: [Integer]
-- tal que sus elementos son los primos de Hilbert. Por ejemplo,
--    take 15 primosH     == [5,9,13,17,21,29,33,37,41,49,53,57,61,69,73]
--    primosH !! (3*10^4) == 313661
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_primos_de_Hilbert where

import Data.Numbers.Primes (isPrime, primeFactors)
import Test.QuickCheck (NonNegative (NonNegative), quickCheck)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª solución
-- ===========

primosH1 :: [Integer]
primosH1 = [n | n <- tail numerosH,
                divisoresH n == [1,n]]

-- numerosH es la sucesión de los números de Hilbert. Por ejemplo,
--    take 15 numerosH  ==  [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57]
numerosH :: [Integer]
numerosH = [1,5..]

-- (divisoresH n) es la lista de los números de Hilbert que dividen a
-- n. Por ejemplo,
--   divisoresH 117  ==  [1,9,13,117]
--   divisoresH  21  ==  [1,21]
divisoresH :: Integer -> [Integer]
divisoresH n = [x | x <- takeWhile (<=n) numerosH,
                    n `mod` x == 0]

-- 2ª solución
-- ===========

primosH2 :: [Integer]
primosH2 = filter esPrimoH (tail numerosH)
  where esPrimoH n = all noDivideAn [5,9..m]
          where noDivideAn x = n `mod` x /= 0
                m            = ceiling (sqrt (fromIntegral n))

-- 3ª solución
-- ===========

-- Basada en la siguiente propiedad: Un primo de Hilbert es un primo
-- de la forma 4n + 1 o un semiprimo de la forma (4a + 3) × (4b + 3)
-- (ver en https://bit.ly/3zq7h4e ).

primosH3 :: [Integer]
primosH3 = [ n | n <- numerosH, isPrime n || semiPrimoH n ]
  where semiPrimoH n = length xs == 2 && all (\x -> (x-3) `mod` 4 == 0) xs
          where xs = primeFactors n

-- Verificación                                                     --
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    take 15 primosH1 `shouldBe` [5,9,13,17,21,29,33,37,41,49,53,57,61,69,73]
  it "e2" $
    take 15 primosH2 `shouldBe` [5,9,13,17,21,29,33,37,41,49,53,57,61,69,73]
  it "e3" $
    take 15 primosH3 `shouldBe` [5,9,13,17,21,29,33,37,41,49,53,57,61,69,73]

-- La verificación es
--    λ> verifica
--
--    3 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_primosH :: NonNegative Int -> Bool
prop_primosH (NonNegative n) =
  all (== primosH1 !! n)
      [primosH2 !! n,
       primosH3 !! n]

-- La comprobación es
--    λ> quickCheck prop_primosH
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosH1 !! 2000
--    16957
--    (2.16 secs, 752,085,752 bytes)
--    λ> primosH2 !! 2000
--    16957
--    (0.03 secs, 19,771,008 bytes)
--    λ> primosH3 !! 2000
--    16957
--    (0.07 secs, 152,029,168 bytes)
--
--    λ> primosH2 !! (3*10^4)
--    313661
--    (1.44 secs, 989,761,888 bytes)
--    λ> primosH3 !! (3*10^4)
--    313661
--    (2.06 secs, 6,554,068,992 bytes)
