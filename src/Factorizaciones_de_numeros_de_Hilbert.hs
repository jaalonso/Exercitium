-- Factorizaciones_de_numeros_de_Hilbert.hs
-- Factorizaciones de números de Hilbert.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-diciembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un [**número de Hilbert**](http://bit.ly/204SW1p) es un entero
-- positivo de la forma 4n+1. Los primeros números de Hilbert son 1, 5,
-- 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, ...
--
-- Un **primo de Hilbert** es un número de Hilbert n que no es divisible
-- por ningún número de Hilbert menor que n (salvo el 1). Los primeros
-- primos de Hilbert son 5, 9, 13, 17, 21, 29, 33, 37, 41, 49, 53, 57,
-- 61, 69, 73, 77, 89, 93, 97, 101, 109, 113, 121, 129, 133, 137, ...
--
-- Definir la función
--    factorizacionesH :: Integer -> [[Integer]]
-- tal que (factorizacionesH n) es la listas de primos de Hilbert cuyo
-- producto es el número de Hilbert n. Por ejemplo,
--   factorizacionesH  25    ==  [[5,5]]
--   factorizacionesH  45    ==  [[5,9]]
--   factorizacionesH 441    ==  [[9,49],[21,21]]
--   factorizacionesH 80109  ==  [[9,9,989],[9,69,129]]
--
-- Comprobar con QuickCheck que todos los números de Hilbert son
-- factorizables como producto de primos de Hilbert (aunque la
-- factorización, como para el 441, puede no ser única).
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Factorizaciones_de_numeros_de_Hilbert where

import Data.Numbers.Primes (isPrime, primeFactors)
import Test.QuickCheck (Positive (Positive), quickCheck)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Numeros_primos_de_Hilbert (primosH1, primosH2, primosH3)

import Data.Traversable (forM)

-- 1ª solución
-- ===========

factorizacionesH1 :: Integer -> [[Integer]]
factorizacionesH1 = aux primosH1
  where
    aux (x:xs) n
      | x == n         = [[n]]
      | x > n          = []
      | n `mod` x == 0 = map (x:) (aux (x:xs) (n `div` x) ) ++ aux xs n
      | otherwise      = aux xs n

-- 2ª solución
-- ===========

factorizacionesH2 :: Integer -> [[Integer]]
factorizacionesH2 = aux primosH2
  where
    aux (x:xs) n
      | x == n         = [[n]]
      | x > n          = []
      | n `mod` x == 0 = map (x:) (aux (x:xs) (n `div` x) ) ++ aux xs n
      | otherwise      = aux xs n

-- 3ª solución
-- ===========

-- Basada en la siguiente propiedad: Un primo de Hilbert es un primo
-- de la forma 4n + 1 o un semiprimo de la forma (4a + 3) × (4b + 3)
-- (ver en https://bit.ly/3zq7h4e ).

factorizacionesH3 :: Integer -> [[Integer]]
factorizacionesH3 = aux primosH3
  where
    aux (x:xs) n
      | x == n         = [[n]]
      | x > n          = []
      | n `mod` x == 0 = map (x:) (aux (x:xs) (n `div` x) ) ++ aux xs n
      | otherwise      = aux xs n

-- Verificación                                                     --
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [[Integer]]) -> Spec
specG factorizacionesH = do
  it "e1" $
    factorizacionesH  25 `shouldBe` [[5,5]]
  it "e2" $
    factorizacionesH  45 `shouldBe` [[5,9]]
  it "e3" $
    factorizacionesH 441 `shouldBe` [[9,49],[21,21]]

spec :: Spec
spec = do
  describe "def. 1" $ specG factorizacionesH1
  describe "def. 2" $ specG factorizacionesH2
  describe "def. 3" $ specG factorizacionesH3

-- La verificación es
--    λ> verifica
--
--    9 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_factorizacionesH :: Positive Integer -> Bool
prop_factorizacionesH (Positive n) =
  all (== factorizacionesH1 m)
      [factorizacionesH2 m,
       factorizacionesH3 m]
  where m = 1 + 4 * n

-- La comprobación es
--    λ> quickCheck prop_factorizacionesH
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> factorizacionesH1 80109
--    [[9,9,989],[9,69,129]]
--    (42.77 secs, 14,899,787,640 bytes)
--    λ> factorizacionesH2 80109
--    [[9,9,989],[9,69,129]]
--    (0.26 secs, 156,051,104 bytes)
--    λ> factorizacionesH3 80109
--    [[9,9,989],[9,69,129]]
--    (0.35 secs, 1,118,236,536 bytes)

-- Propiedad de factorización
-- ==========================

-- La propiedad es
prop_factorizable :: Positive Integer -> Bool
prop_factorizable (Positive n) =
  not (null (factorizacionesH1 (1 + 4 * n)))

-- La comprobación es
--    λ> quickCheck prop_factorizable
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el artículo [Failure of unique factorization (A simple
-- example of the failure of the fundamental theorem of
-- arithmetic)](http://bit.ly/20A2Nyc) de R.J. Lipton en el blog [Gödel's
-- Lost Letter and P=NP](https://rjlipton.wordpress.com).
--
-- Otras  referencias
--
-- + Wikipedia, [Hilbert number](http://bit.ly/204SW1p).
-- + E.W. Weisstein, [Hilbert number](http://bit.ly/204T8O4) en MathWorld.
-- + N.J.A. Sloane, [Sucesión A057948](https://oeis.org/A057948) en la
--   OEIS.
-- + N.J.A. Sloane, [Sucesión A057949](https://oeis.org/A057949) en la
--   OEIS.
