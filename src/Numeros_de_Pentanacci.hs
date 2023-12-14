-- Numeros_de_Pentanacci.hs
-- Números de Pentanacci.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-agosto-2022 (versión 14-dic-23)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números de Fibonacci se definen mediante las ecuaciones
--    F(0) = 0
--    F(1) = 1
--    F(n) = F(n-1) + F(n-2), si n > 1
-- Los primeros números de Fibonacci son
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, ...
--
-- Una generalización de los anteriores son los números de Pentanacci
-- definidos por las siguientes ecuaciones
--    P(0) = 0
--    P(1) = 1
--    P(2) = 1
--    P(3) = 2
--    P(4) = 4
--    P(n) = P(n-1) + P(n-2) + P(n-3) + P(n-4) + P(n-5), si n > 4
-- Los primeros números de Pentanacci son
--   0, 1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, ...
--
-- Definir las funciones
--    pentanacci  :: Integer -> Integer
--    pentanaccis :: [Integer]
-- tales que
-- + (pentanacci n) es el n-ésimo número de Pentanacci. Por ejemplo,
--      λ> pentanacci 14
--      3525
--      λ> pentanacci (10^5) `mod` 10^30
--      482929150584077921552549215816
--      λ> length (show (pentanacci (10^5)))
--      29357
-- + pentanaccis es la lista de los números de Pentanacci. Por ejemplo,
--      λ> take 15 pentanacci
--      [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_de_Pentanacci where

import Data.List (genericIndex, zipWith5)
import Test.Hspec (Spec, hspec, it, shouldBe)
import Test.QuickCheck (NonNegative (NonNegative), quickCheckWith, maxSize, stdArgs)

-- 1ª solución
-- ===========

pentanacci1  :: Integer -> Integer
pentanacci1  0 = 0
pentanacci1  1 = 1
pentanacci1  2 = 1
pentanacci1  3 = 2
pentanacci1  4 = 4
pentanacci1  n = sum [pentanacci1 (n-k) | k <- [1..5]]

pentanaccis1 :: [Integer]
pentanaccis1 = [pentanacci1 n | n <- [0..]]

-- 2ª solución
-- ===========

pentanaccis2 :: [Integer]
pentanaccis2 =
  0 : 1 : 1 : 2 : 4 : zipWith5 f (r 0) (r 1) (r 2) (r 3) (r 4)
  where f a b c d e = a+b+c+d+e
        r n         = drop n pentanaccis2

pentanacci2  :: Integer -> Integer
pentanacci2 n = pentanaccis2 `genericIndex` n

-- 3ª solución
-- ===========

pentanaccis3 :: [Integer]
pentanaccis3 = p (0, 1, 1, 2, 4)
  where p (a, b, c, d, e) = a : p (b, c, d, e, a + b + c + d + e)

pentanacci3  :: Integer -> Integer
pentanacci3 n = pentanaccis3 `genericIndex` n

-- 4ª solución
-- ===========

pentanaccis4 :: [Integer]
pentanaccis4 = 0: 1: 1: 2: 4: p pentanaccis4
  where p (a:b:c:d:e:xs) = (a+b+c+d+e): p (b:c:d:e:xs)

pentanacci4  :: Integer -> Integer
pentanacci4 n = pentanaccis4 `genericIndex` n

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "ej1" $
    take 15 pentanaccis1 `shouldBe`
    [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]
  it "ej2" $
    take 15 pentanaccis2 `shouldBe`
    [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]
  it "ej3" $
    take 15 pentanaccis3 `shouldBe`
    [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]
  it "ej4" $
    take 15 pentanaccis4 `shouldBe`
    [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]

-- La verificación es
--    λ> verifica
--
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_pentanaccis :: NonNegative Int -> Bool
prop_pentanaccis (NonNegative n) =
  all (== pentanaccis1 !! n)
      [pentanaccis1 !! n,
       pentanaccis2 !! n,
       pentanaccis3 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=25}) prop_pentanaccis
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> pentanacci1 25
--    5976577
--    (4.64 secs, 1,865,626,496 bytes)
--    λ> pentanacci2 25
--    5976577
--    (0.00 secs, 578,584 bytes)
--
--    λ> length (show (pentanacci2 (10^5)))
--    29357
--    (1.16 secs, 2,543,272,136 bytes)
--    λ> length (show (pentanacci3 (10^5)))
--    29357
--    (1.00 secs, 2,560,881,608 bytes)
--    λ> length (show (pentanacci4 (10^5)))
--    29357
--    (1.03 secs, 2,592,078,744 bytes)

-- Referencias
-- ===========

-- + Tito III Piezas y Eric Weisstein, [Pentanacci number](https://bit.ly/3cPJGkF).
-- + N. J. A. Sloane, [Sucesión A001591 de la OEIS](https://oeis.org/A001591).
