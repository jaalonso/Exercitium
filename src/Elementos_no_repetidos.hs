-- Elementos_no_repetidos.hs
-- Elementos no repetidos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-Noviembre-2014 (actualizado 22-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    noRepetidos :: Eq a => [a] -> [a]
-- tal que (noRepetidos xs) es la lista de los elementos no repetidos de
-- la lista xs. Por ejemplo,
--    noRepetidos [3,2,5,2,4,7,3]  ==  [5,4,7]
--    noRepetidos "noRepetidos"    ==  "nRptids"
--    noRepetidos "Roma"           ==  "Roma"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elementos_no_repetidos where

import Data.Map as M (Map, empty, findWithDefault, fromListWith, insertWith, lookup)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

noRepetidos1 :: Eq a => [a] -> [a]
noRepetidos1 xs =
 [x | x <- xs, ocurrencias x xs == 1]

-- (ocurrencias x ys) es el número de ocurrencias de x en ys. Por
-- ejemplo,
--    ocurrencias 3 [3,2,5,2,4,7,3]  ==  2
--    ocurrencias 5 [3,2,5,2,4,7,3]  ==  1
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias x ys =
 length [y | y <- ys, y == x]

-- 2ª solución
-- ===========

noRepetidos2 :: Eq a => [a] -> [a]
noRepetidos2 [] = []
noRepetidos2 (x:xs) | elem x xs = noRepetidos2 [y | y <- xs, y /= x]
                    | otherwise = x : noRepetidos2 xs

-- 3ª solución
-- ===========

noRepetidos3 :: Ord a => [a] -> [a]
noRepetidos3 xs =
  [x | x <- xs, M.lookup x d == Just 1]
  where d = dicOcurrencias xs

-- (dicOcurrencias xs) es el diccionario formado por los elementos de xs
-- junto con el número de sus ocurrencias en xs. Por ejemplo,
--    λ> dicOcurrencias [3,2,5,2,4,7,3]
--    fromList [(2,2),(3,2),(4,1),(5,1),(7,1)]
dicOcurrencias :: Ord a => [a] -> Map a Int
dicOcurrencias xs = fromListWith (+) [(x, 1) | x <- xs]

-- 4ª solución
-- ===========

noRepetidos4 :: Ord a => [a] -> [a]
noRepetidos4 xs =
  [x | x <- xs, findWithDefault 1 x d == 1]
  where d = dicOcurrencias2 xs

dicOcurrencias2 :: Ord a => [a] -> Map a Int
dicOcurrencias2 =
  foldr (\x d -> insertWith (+) x 1 d) empty

-- 5ª solución
-- ===========

noRepetidos5 :: Ord a => [a] -> [a]
noRepetidos5 xs = aux xs
  where
    d = dicOcurrencias2 xs
    aux [] = []
    aux (y:ys)
      | findWithDefault 0 y d == 1 = y : aux ys
      | otherwise                  = aux ys

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG noRepetidos = do
  it "e1" $
    noRepetidos [3,2,5,2,4,7,3] `shouldBe` [5,4,7]

spec :: Spec
spec = do
  describe "def. 1" $ specG noRepetidos1
  describe "def. 2" $ specG noRepetidos2
  describe "def. 3" $ specG noRepetidos3
  describe "def. 4" $ specG noRepetidos4
  describe "def. 5" $ specG noRepetidos5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad de equivalencia es
prop_noRepetidos :: [Int] -> Bool
prop_noRepetidos xs =
  all (== noRepetidos1 xs)
      [noRepetidos2 xs,
       noRepetidos3 xs,
       noRepetidos4 xs,
       noRepetidos5 xs]

-- La comprobación es
--    λ> quickCheck prop_noRepetidos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (noRepetidos1 ([1..1000] ++ [500..4000]))
--    3499
--    (2.85 secs, 1,137,498,936 bytes)
--    λ> length (noRepetidos2 ([1..1000] ++ [500..4000]))
--    3499
--    (0.38 secs, 327,159,984 bytes)
--    λ> length (noRepetidos3 ([1..1000] ++ [500..4000]))
--    3499
--    (0.04 secs, 6,064,784 bytes)
--    λ> length (noRepetidos4 ([1..1000] ++ [500..4000]))
--    3499
--    (0.03 secs, 5,988,992 bytes)
--    λ> length (noRepetidos5 ([1..1000] ++ [500..4000]))
--    3499
--    (0.03 secs, 6,233,016 bytes)
--
--    λ> length (noRepetidos2 ([1..1000] ++ [500..20000]))
--    19499
--    (3.09 secs, 1,882,444,712 bytes)
--    λ> length (noRepetidos3 ([1..1000] ++ [500..20000]))
--    19499
--    (0.07 secs, 29,240,808 bytes)
--    λ> length (noRepetidos4 ([1..1000] ++ [500..20000]))
--    19499
--    (0.03 secs, 31,419,192 bytes)
--    λ> length (noRepetidos5 ([1..1000] ++ [500..20000]))
--    19499
--    (0.07 secs, 32,559,216 bytes)
--
--    λ> length (noRepetidos3 ([1..1000] ++ [500..600000]))
--    599499
--    (1.10 secs, 1,057,491,808 bytes)
--    λ> length (noRepetidos4 ([1..1000] ++ [500..600000]))
--    599499
--    (2.19 secs, 1,459,836,336 bytes)
--    λ> length (noRepetidos5 ([1..1000] ++ [500..600000]))
--    599499
--    (2.24 secs, 1,493,456,360 bytes)
