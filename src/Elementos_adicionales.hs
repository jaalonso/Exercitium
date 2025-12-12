-- Elementos_adicionales.hs
-- Elementos adicionales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-Diciembre-2014 (actualizado 11-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    adicionales :: Ord a => Int -> [a] -> [a] -> [a]
-- tal que (adicionales n xs ys) es la lista de los n elementos de xs
-- que no pertenecen a ys (se supone que n rd rl número de elementos de
-- xs que no pertenecen a ys, que las listas xs e ys están estrictamente
-- ordenadas y que pueden ser infinitas). Por ejemplo,
--    adicionales 0 [1,3]   [1,3]                  ==  []
--    adicionales 1 [1,3]   [1]                    ==  [3]
--    adicionales 2 [1,3,5] [1]                    ==  [3,5]
--    adicionales 2 [1,3,5,7,9] [1,5,7]            ==  [3,9]
--    adicionales 2 [1,3,5,7,9] [1,5]              ==  [3,7]
--    adicionales 2 ([1,3,5]++[7..]) ([1]++[7..])  ==  [3,5]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Elementos_adicionales where

import Data.List ((\\), nub, sort)
import Data.List.Ordered (minus)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

adicionales1 :: Ord a => Int -> [a] -> [a] -> [a]
adicionales1 0 _  _  = []
adicionales1 _ xs [] = xs
adicionales1 n (x:xs) (y:ys)
  | x <  y    = x : adicionales1 (n-1) xs (y:ys)
  | x == y    = adicionales1 n xs ys
  | otherwise = adicionales1 n (x:xs) ys

-- 2ª solución
-- ===========

adicionales2 :: Ord a => Int -> [a] -> [a] -> [a]
adicionales2 n xs ys =
  take n [x | x <- xs, x `noPertenece` ys]

-- (noPertenece x ys) se verifica si x no pertenece a la lista ordenada
-- (posiblemente infinita ys). Por ejemplo.
--    noPertenece 2 [3,5]    ==  True
--    noPertenece 4 [3,5]    ==  True
--    noPertenece 7 [3,5]    ==  True
--    noPertenece 4 [3,5..]  ==  True
noPertenece :: Ord a => a -> [a] -> Bool
noPertenece x ys = null zs || head zs /= x
  where zs = dropWhile (<x) ys

-- 3ª solución
-- ===========

adicionales3 :: Ord a => Int -> [a] -> [a] -> [a]
adicionales3 n xs ys = take n (minus xs ys)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> [Int] -> [Int]) -> Spec
specG adicionales = do
  it "e1" $
    adicionales 0 [1,3]   [1,3]        `shouldBe`  []
  it "e2" $
    adicionales 1 [1,3]   [1]          `shouldBe`  [3]
  it "e3" $
    adicionales 2 [1,3,5] [1]          `shouldBe`  [3,5]
  it "e4" $
    adicionales 2 [1,3,5,7,9] [1,5,7]  `shouldBe`  [3,9]
  it "e5" $
    adicionales 2 ([1,3,5]++[7..]) (1 : [7..])  `shouldBe`  [3,5]

spec :: Spec
spec = do
  describe "def. 1" $ specG adicionales1
  describe "def. 2" $ specG adicionales2
  describe "def. 3" $ specG adicionales3

-- La verificación es
--    λ> verifica
--    18 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- genEjemplo es un generador de ejemplos de entrada. Por ejemplo,
--    λ> generate genEjemplo
--    (3,[-18,-13,-9,-1,12,16],[-13,-9,-1])
genEjemplo :: Gen (Int, [Int], [Int])
genEjemplo = do
  n <- choose (0,5)
  m <- choose (n,10)
  as <- vectorOf n arbitrary
  bs <- vectorOf m arbitrary
  let as' = nub as
      bs' = nub bs
      n'  = length as'
      ys = sort (bs' \\ as')
      xs = sort (as' ++ ys)
  return (n', xs, ys)

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia =
  forAll genEjemplo $ \(n, xs, ys) ->
    adicionales1 n xs ys == adicionales2 n xs ys

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> adicionales1 1 ([1..6000]++[0]) [1..6000]
--    [0]
--    (0.01 secs, 3,142,040 bytes)
--    λ> adicionales2 1 ([1..6000]++[0]) [1..6000]
--    [0]
--    (1.50 secs, 5,254,744 bytes)
