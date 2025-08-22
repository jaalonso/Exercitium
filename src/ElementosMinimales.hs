-- ElementosMinimales.hs
-- Determinación de los elementos minimales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-abril-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    minimales :: Eq a => [[a]] -> [[a]]
-- tal que (minimales xss) es la lista de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
--    minimales [[1,3],[2,3,1],[3,2,5]]        ==  [[2,3,1],[3,2,5]]
--    minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  ==  [[2,3,1],[3,2,5]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ElementosMinimales where

import Data.List (nub, delete, (\\))
import Data.Set (isSubsetOf, fromList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

minimales1 :: Eq a => [[a]] -> [[a]]
minimales1 xss =
  [xs | xs <- xss,
        null [ys | ys <- xss, subconjuntoPropio xs ys]]

-- (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo,
--    subconjuntoPropio [1,3] [3,1,3]    ==  False
--    subconjuntoPropio [1,3,1] [3,1,2]  ==  True
subconjuntoPropio :: Eq a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = subconjuntoPropio' (nub xs) (nub ys)
  where
    subconjuntoPropio' _ [] = False
    subconjuntoPropio' [] _ = True
    subconjuntoPropio' (x:xs') ys' =
      x `elem` ys' && subconjuntoPropio xs' (delete x ys')

-- 2ª solución
-- ===========

minimales2 :: Eq a => [[a]] -> [[a]]
minimales2 xss = filter noTieneSubconjunto xss
  where
    noTieneSubconjunto xs = not (any (subconjuntoPropio xs) xss)

-- 3ª solución
-- ===========

minimales3 :: Eq a => [[a]] -> [[a]]
minimales3 xss =
  [xs | xs <- xss, all (not . subconjuntoPropio2 xs) xss]

subconjuntoPropio2 :: Eq a => [a] -> [a] -> Bool
subconjuntoPropio2 xs ys =
  xs' /= ys' && null (xs' \\ ys')
  where xs' = nub xs
        ys' = nub ys

-- 4ª solución
-- ===========

minimales4 :: Eq a => [[a]] -> [[a]]
minimales4 xss =
  [xs | xs <- xss, not (any (subconjuntoPropio2 xs) xss)]

-- 5ª solución
-- ===========

minimales5 :: Ord a => [[a]] -> [[a]]
minimales5 xss =
  [xs | xs <- xss, (not . any (subconjuntoPropio3 xs)) xss]

subconjuntoPropio3 :: Ord a => [a] -> [a] -> Bool
subconjuntoPropio3 xs ys =
  setXs /= setYs && setXs `isSubsetOf` setYs
  where setXs = fromList xs
        setYs = fromList ys

-- 6ª solución
-- ===========

minimales6 :: Eq a => [[a]] -> [[a]]
minimales6 = foldr agregarSiMinimal []
  where
    agregarSiMinimal xs yss
      | any (subconjuntoPropio xs) yss = yss
      | otherwise = xs : filter (not . flip subconjuntoPropio xs) yss

-- 7ª solución
-- ===========

minimales7 :: Eq a => [[a]] -> [[a]]
minimales7 xss =
  [xs | xs <- xss,
        not (or [subconjuntoPropio xs ys | ys <- xss])]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG minimales = do
  it "e1" $
    minimales [[1,3],[2,3,1],[3,2,5]]        `shouldBe`  [[2,3,1],[3,2,5]]
  it "e2" $
    minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  `shouldBe`  [[2,3,1],[3,2,5]]

spec :: Spec
spec = do
  describe "def. 1" $ specG minimales1
  describe "def. 2" $ specG minimales2
  describe "def. 3" $ specG minimales3
  describe "def. 4" $ specG minimales4
  describe "def. 5" $ specG minimales5
  describe "def. 6" $ specG minimales6
  describe "def. 7" $ specG minimales7

-- La verificación es
--    λ> verifica
--    14 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_minimales :: [[Int]] -> Bool
prop_minimales xss =
  all (== minimales1 xss)
      [minimales2 xss,
       minimales3 xss,
       minimales4 xss,
       minimales5 xss,
       minimales6 xss,
       minimales7 xss]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=40}) prop_minimales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (minimales1 [[1..n] | n <- [1..100]])
--    1
--    (2.27 secs, 973,011,248 bytes)
--    λ> length (minimales2 [[1..n] | n <- [1..100]])
--    1
--    (2.19 secs, 973,170,440 bytes)
--    λ> length (minimales3 [[1..n] | n <- [1..100]])
--    1
--    (0.14 secs, 37,972,696 bytes)
--    λ> length (minimales4 [[1..n] | n <- [1..100]])
--    1
--    (0.12 secs, 37,804,728 bytes)
--    λ> length (minimales5 [[1..n] | n <- [1..100]])
--    1
--    (0.05 secs, 39,607,648 bytes)
--    λ> length (minimales6 [[1..n] | n <- [1..100]])
--    1
--    (0.12 secs, 36,018,976 bytes)
--    λ> length (minimales7 [[1..n] | n <- [1..100]])
--    1
--    (2.16 secs, 973,840,760 bytes)
--
--    λ> length (minimales3 [[1..n] | n <- [1..250]])
--    1
--    (3.47 secs, 533,688,496 bytes)
--    λ> length (minimales4 [[1..n] | n <- [1..250]])
--    1
--    (3.46 secs, 532,668,528 bytes)
--    λ> length (minimales5 [[1..n] | n <- [1..250]])
--    1
--    (0.27 secs, 579,544,232 bytes)
--    λ> length (minimales6 [[1..n] | n <- [1..250]])
--    1
--    (3.30 secs, 740,152,496 bytes)
