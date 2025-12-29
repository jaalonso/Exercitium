-- Listas_disjuntas.hs
-- Listas disjuntas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-Enero-2015 (actualizado 28-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    disjuntas :: Ord a => [a] -> [a] -> Bool
-- tal que (disjuntas xs ys) se verifica si las listas ordenadas
-- crecientemente xs e ys son disjuntas. Por ejemplo,
--    disjuntas [2,5]   [1,4,7]                  ==  True
--    disjuntas [2,5,7] [1,4,7]                  ==  False
--    disjuntas [1..1000000] [3000000..4000000]  ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Listas_disjuntas where

import Data.Set (disjoint, fromDistinctAscList)
import Data.List.Ordered (isect)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

disjuntas1 :: Ord a => [a] -> [a] -> Bool
disjuntas1 [] _ = True
disjuntas1 _ [] = True
disjuntas1 (x:xs) (y:ys)
  | x < y     = disjuntas1 xs (y:ys)
  | x > y     = disjuntas1 (x:xs) ys
  | otherwise = False

-- 2ª solución
-- ===========

disjuntas2 :: Ord a => [a] -> [a] -> Bool
disjuntas2 xs'@(x:xs) ys'@(y:ys)
  | x < y     = disjuntas2 xs ys'
  | x > y     = disjuntas2 xs' ys
  | otherwise = False
disjuntas2 _ _ = True

-- 3ª solución
-- ===========

disjuntas3 :: Ord a => [a] -> [a] -> Bool
disjuntas3 [] _ = True
disjuntas3 _ [] = True
disjuntas3 (x:xs) (y:ys) =
  case compare x y of
    LT -> disjuntas3 xs (y:ys)
    GT -> disjuntas3 (x:xs) ys
    EQ -> False

-- 4ª solución
-- ===========

disjuntas4 :: Ord a => [a] -> [a] -> Bool
disjuntas4 xs ys =
  disjoint (fromDistinctAscList xs) (fromDistinctAscList ys)

-- 5ª solución
-- ===========

disjuntas5 :: Ord a => [a] -> [a] -> Bool
disjuntas5 xs ys = null (isect xs ys)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int] -> Bool) -> Spec
specG disjuntas = do
  it "e1" $
    disjuntas [2,5]   [1,4,7]                  `shouldBe`  True
  it "e2" $
    disjuntas [2,5,7] [1,4,7]                  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG disjuntas1
  describe "def. 2" $ specG disjuntas2
  describe "def. 3" $ specG disjuntas3
  describe "def. 4" $ specG disjuntas4
  describe "def. 5" $ specG disjuntas5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: OrderedList Int -> OrderedList Int -> Bool
prop_equivalencia (Ordered xs) (Ordered ys) =
  all (== disjuntas1 xs ys)
      [disjuntas2 xs ys,
       disjuntas3 xs ys,
       disjuntas4 xs ys,
       disjuntas5 xs ys]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> disjuntas1 [1..10^7] [3*10^7..4*10^7]
--    True
--    (3.57 secs, 2,640,601,080 bytes)
--    λ> disjuntas2 [1..10^7] [3*10^7..4*10^7]
--    True
--    (3.18 secs, 1,920,601,080 bytes)
--    λ> disjuntas3 [1..10^7] [3*10^7..4*10^7]
--    True
--    (4.17 secs, 3,360,601,080 bytes)
--    λ> disjuntas4 [1..10^7] [3*10^7..4*10^7]
--    True
--    (4.16 secs, 2,240,620,912 bytes)
--    λ> disjuntas5 [1..10^7] [3*10^7..4*10^7]
--    True
--    (0.29 secs, 720,601,152 bytes)
