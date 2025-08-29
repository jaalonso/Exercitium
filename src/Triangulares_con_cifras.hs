-- Triangulares_con_cifras.hs
-- Números triangulares con n cifras distintas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-Mayo-2014 (actualozado 29-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números triangulares se forman como sigue
--
--    *     *      *
--         * *    * *
--               * * *
--    1     3      6
--
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
--     1 = 1
--     3 = 1 + 2
--     6 = 1 + 2 + 3
--    10 = 1 + 2 + 3 + 4
--    15 = 1 + 2 + 3 + 4 + 5
--
-- Definir la función
--    triangularesConCifras :: Int -> [Integer]
-- tal que (triangularesConCifras n) es la lista de los números
-- triangulares con n cifras distintas. Por  ejemplo,
--    take 6 (triangularesConCifras 1)   ==  [1,3,6,55,66,666]
--    take 6 (triangularesConCifras 2)   ==  [10,15,21,28,36,45]
--    take 6 (triangularesConCifras 3)   ==  [105,120,136,153,190,210]
--    take 5 (triangularesConCifras 4)   ==  [1035,1275,1326,1378,1485]
--    take 2 (triangularesConCifras 10)  ==  [1062489753,1239845706]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Triangulares_con_cifras where

import Data.List (nub)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

triangularesConCifras1 :: Int -> [Integer]
triangularesConCifras1 n =
  [x | x <- triangulares1,
       nCifras x == n]

-- triangulares1 es la lista de los números triangulares. Por ejemplo,
--    take 10 triangulares1 == [1,3,6,10,15,21,28,36,45,55]
triangulares1 :: [Integer]
triangulares1 = map triangular [1..]

triangular :: Integer -> Integer
triangular 1 = 1
triangular n = triangular (n-1) + n

-- (nCifras x) es el número de cifras distintas del número x. Por
-- ejemplo,
--    nCifras 325275  ==  4
nCifras :: Integer -> Int
nCifras = length . nub . show

-- 2ª solución
-- ===========

triangularesConCifras2 :: Int -> [Integer]
triangularesConCifras2 n =
  [x | x <- triangulares2,
       nCifras x == n]

triangulares2 :: [Integer]
triangulares2 = [(n*(n+1)) `div` 2 | n <- [1..]]

-- 3ª solución
-- ===========

triangularesConCifras3 :: Int -> [Integer]
triangularesConCifras3 n =
  [x | x <- triangulares3,
       nCifras x == n]

triangulares3 :: [Integer]
triangulares3 = 1 : [x+y | (x,y) <- zip [2..] triangulares3]

-- 4ª solución
-- ===========

triangularesConCifras4 :: Int -> [Integer]
triangularesConCifras4 n =
  [x | x <- triangulares4,
       nCifras x == n]

triangulares4 :: [Integer]
triangulares4 = 1 : zipWith (+) [2..] triangulares4

-- 5ª solución
-- ===========

triangularesConCifras5 :: Int -> [Integer]
triangularesConCifras5 n =
  [x | x <- triangulares5,
       nCifras x == n]

triangulares5 :: [Integer]
triangulares5 = scanl (+) 1 [2..]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Integer]) -> Spec
specG triangularesConCifras = do
  it "e1" $
    take 6 (triangularesConCifras 1) `shouldBe` [1,3,6,55,66,666]
  it "e2" $
    take 6 (triangularesConCifras 2) `shouldBe` [10,15,21,28,36,45]
  it "e3" $
    take 6 (triangularesConCifras 3) `shouldBe` [105,120,136,153,190,210]
  it "e4" $
    take 5 (triangularesConCifras 4) `shouldBe` [1035,1275,1326,1378,1485]

spec :: Spec
spec = do
  describe "def. 1" $ specG triangularesConCifras1
  describe "def. 2" $ specG triangularesConCifras2
  describe "def. 3" $ specG triangularesConCifras3
  describe "def. 4" $ specG triangularesConCifras4
  describe "def. 5" $ specG triangularesConCifras5

-- La verificación es
--    λ> verifica
--    20 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La 1ª propiedad es
prop_triangularesConCifras1 :: Bool
prop_triangularesConCifras1 =
  [take 2 (triangularesConCifras1 n) | n <- [1..7]] ==
  [take 2 (triangularesConCifras2 n) | n <- [1..7]]

-- La comprobación es
--    λ> prop_triangularesConCifras1
--    True

-- La 2ª propiedad es
prop_triangularesConCifras2 :: Int -> Bool
prop_triangularesConCifras2 n =
  all (== take 5 (triangularesConCifras2 n'))
      [take 5 (triangularesConCifras3 n'),
       take 5 (triangularesConCifras4 n'),
       take 5 (triangularesConCifras5 n')]
  where n' = 1 + n `mod` 9

-- La comprobación es
--    λ> quickCheck prop_triangularesConCifras
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> (triangularesConCifras1 3) !! 220
--    5456556
--    (2.48 secs, 1,228,690,120 bytes)
--    λ> (triangularesConCifras2 3) !! 220
--    5456556
--    (0.01 secs, 4,667,288 bytes)
--
--    λ> (triangularesConCifras2 3) !! 600
--    500010500055
--    (1.76 secs, 1,659,299,872 bytes)
--    λ> (triangularesConCifras3 3) !! 600
--    500010500055
--    (1.67 secs, 1,603,298,648 bytes)
--    λ> (triangularesConCifras4 3) !! 600
--    500010500055
--    (1.20 secs, 1,507,298,248 bytes)
--    λ> (triangularesConCifras5 3) !! 600
--    500010500055
--    (1.15 secs, 1,507,298,256 bytes)

-- ---------------------------------------------------------------------
-- §  Referencias                                                     --
-- ---------------------------------------------------------------------

-- + M. Keith [On repdigit polygonal numbers](http://bit.ly/1hzZrDk).
-- + OEIS [A045914: Triangular numbers with all digits the
--   same](https://oeis.org/A045914)
-- + OEIS [A213516: Triangular numbers having only 1 or 2 different
--   digits in base 10](https://oeis.org/A213516).
-- + [Series of natural numbers which has all same
--   digits](http://bit.ly/1hA0Sl4).
