-- Diferencia_simetrica.hs
-- Diferencia simétrica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-mayo-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La [diferencia simétrica](http://bit.ly/1Rdcqxs) de dos conjuntos es
-- el conjunto cuyos elementos son aquellos que pertenecen a alguno de
-- los conjuntos iniciales, sin pertenecer a ambos a la vez. Por
-- ejemplo, la diferencia simétrica de {2,5,3} y {4,2,3,7} es {5,4,7}.
--
-- Definir la función
--    diferenciaSimetrica :: Ord a => [a] -> [a] -> [a]
-- tal que (diferenciaSimetrica xs ys) es la diferencia simétrica de xs
-- e ys. Por ejemplo,
--    diferenciaSimetrica [2,5,3] [4,2,3,7]    ==  [4,5,7]
--    diferenciaSimetrica [2,5,3] [5,2,3]      ==  []
--    diferenciaSimetrica [2,5,2] [4,2,3,7]    ==  [3,4,5,7]
--    diferenciaSimetrica [2,5,2] [4,2,4,7]    ==  [4,5,7]
--    diferenciaSimetrica [2,5,2,4] [4,2,4,7]  ==  [5,7]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Diferencia_simetrica where

import Data.List ((\\), intersect, nub, sort, union)
import qualified Data.Set as S
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

diferenciaSimetrica1 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica1 xs ys =
  sort (nub ([x | x <- xs, x `notElem` ys] ++
             [y | y <- ys, y `notElem` xs]))

-- 2ª solución
-- ===========

diferenciaSimetrica2 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica2 xs ys =
  sort (nub (filter (`notElem` ys) xs ++
             filter (`notElem` xs) ys))

-- 3ª solución
-- ===========

diferenciaSimetrica3 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica3 xs ys =
  sort (nub (union xs ys \\ intersect xs ys))

-- 4ª solución
-- ===========

diferenciaSimetrica4 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica4 xs ys =
  [x | x <- sort (nub (xs ++ ys))
     , x `notElem` xs || x `notElem` ys]

-- 5ª solución
-- ===========

diferenciaSimetrica5 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica5 xs ys =
  S.elems ((xs' `S.union` ys') `S.difference` (xs' `S.intersection` ys'))
  where xs' = S.fromList xs
        ys' = S.fromList ys

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG diferenciaSimetrica = do
  it "e1" $
    diferenciaSimetrica [2,5,3] [4,2,3,7]    `shouldBe`  [4,5,7]
  it "e2" $
    diferenciaSimetrica [2,5,3] [5,2,3]      `shouldBe`  []
  it "e3" $
    diferenciaSimetrica [2,5,2] [4,2,3,7]    `shouldBe`  [3,4,5,7]
  it "e4" $
    diferenciaSimetrica [2,5,2] [4,2,4,7]    `shouldBe`  [4,5,7]
  it "e5" $
    diferenciaSimetrica [2,5,2,4] [4,2,4,7]  `shouldBe`  [5,7]

spec :: Spec
spec = do
  describe "def. 1" $ specG diferenciaSimetrica1
  describe "def. 2" $ specG diferenciaSimetrica2
  describe "def. 3" $ specG diferenciaSimetrica3
  describe "def. 4" $ specG diferenciaSimetrica4
  describe "def. 5" $ specG diferenciaSimetrica5

-- La verificación es
--    λ> verifica
--    25 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_diferenciaSimetrica :: [Int] -> [Int] -> Bool
prop_diferenciaSimetrica xs ys =
  all (== diferenciaSimetrica1 xs ys)
      [diferenciaSimetrica2 xs ys,
       diferenciaSimetrica3 xs ys,
       diferenciaSimetrica4 xs ys,
       diferenciaSimetrica5 xs ys]

-- La comprobación es
--    λ> quickCheck prop_diferenciaSimetrica
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (diferenciaSimetrica1 [1..2*10^4] [2,4..2*10^4])
--    10000
--    (2.34 secs, 10,014,360 bytes)
--    λ> length (diferenciaSimetrica2 [1..2*10^4] [2,4..2*10^4])
--    10000
--    (2.41 secs, 8,174,264 bytes)
--    λ> length (diferenciaSimetrica3 [1..2*10^4] [2,4..2*10^4])
--    10000
--    (5.84 secs, 10,232,006,288 bytes)
--    λ> length (diferenciaSimetrica4 [1..2*10^4] [2,4..2*10^4])
--    10000
--    (5.83 secs, 14,814,184 bytes)
--    λ> length (diferenciaSimetrica5 [1..2*10^4] [2,4..2*10^4])
--    10000
--    (0.02 secs, 7,253,496 bytes)
