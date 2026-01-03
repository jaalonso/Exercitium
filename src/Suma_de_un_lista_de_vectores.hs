-- Suma_de_un_lista_de_vectores.hs
-- Suma de una lista de vectores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-Enero-2015 (actualizado 3-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaVec :: Num a => [[a]] -> [a]
-- tal que (sumaVec xss) es la suma de los vectores de xss. Por ejemplo,
--    sumaVec [[4,7,3],[3,1,4],[2,2,5]]  ==  [9,10,12]
--    sumaVec [[4,7],[3,3],[1,4],[2,5]]  ==  [10,19]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Suma_de_un_lista_de_vectores where

import Data.List (foldl', transpose)
import Data.Array (listArray,(!))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaVec1 :: Num a => [[a]] -> [a]
sumaVec1 []          = []
sumaVec1 [xs]        = xs
sumaVec1 (xs:ys:zss) = suma xs (sumaVec1 (ys:zss))

-- (suma xs ys) es la suma de los vectores xs e ys. Por ejemplo,
--    suma [4,7,3] [1,2,5]  == [5,9,8]
suma :: Num a => [a] -> [a] -> [a]
suma (x:xs) (y:ys) = x+y : suma xs ys
suma _ _          = []

-- 2ª solución
-- ===========

sumaVec2 :: Num a => [[a]] -> [a]
sumaVec2 []       = []
sumaVec2 [xs]     = xs
sumaVec2 (xs:xss) = zipWith (+) xs (sumaVec2 xss)

-- 3ª solución
-- ===========

sumaVec3 :: Num a => [[a]] -> [a]
sumaVec3 = foldl1 (zipWith (+))

-- 4ª solución
-- ===========

sumaVec4 :: Num a => [[a]] -> [a]
sumaVec4 xss = [sum xs | xs <- transpose xss]

-- 5ª solución
-- ===========

sumaVec5 :: Num a => [[a]] -> [a]
sumaVec5 = map sum . transpose

-- 6ª solución
-- ===========

sumaVec6 :: Num a => [[a]] -> [a]
sumaVec6 []       = []
sumaVec6 (xs:xss) = foldl' (zipWith (+)) xs xss

-- 7ª solución
-- ===========

sumaVec7 :: Num a => [[a]] -> [a]
sumaVec7 xss = [sum [p!(i,j) | i <- [1..m]] | j <- [1..n]]
  where m = length xss
        n = length (head xss)
        p = listArray ((1,1),(m,n)) (concat xss)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [Int]) -> Spec
specG sumaVec = do
  it "e1" $
    sumaVec [[4,7,3],[3,1,4],[2,2,5]] `shouldBe` [9,10,12]
  it "e2" $
    sumaVec [[4,7],[3,3],[1,4],[2,5]] `shouldBe` [10,19]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaVec1
  describe "def. 2" $ specG sumaVec2
  describe "def. 3" $ specG sumaVec3
  describe "def. 4" $ specG sumaVec4
  describe "def. 5" $ specG sumaVec5
  describe "def. 6" $ specG sumaVec6
  describe "def. 7" $ specG sumaVec7

-- La verificación es
--    λ> verifica
--    14 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Genera una matriz. Por ejemplo,
--    λ> generate genMatriz
--    [[15,22],[29,12],[-28,-1]]
genMatriz :: Gen [[Int]]
genMatriz = do
  m <- choose (1, 20)
  n  <- choose (1, 20)
  vectorOf m (vectorOf n (arbitrary :: Gen Int))

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia = forAll genMatriz $ \xss ->
  all (== sumaVec1 xss)
      [sumaVec2 xss,
       sumaVec4 xss,
       sumaVec5 xss,
       sumaVec6 xss,
       sumaVec7 xss]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> sum (sumaVec1 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (7.04 secs, 9,041,786,784 bytes)
--    λ> sum (sumaVec2 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (1.56 secs, 5,131,459,576 bytes)
--    λ> sum (sumaVec3 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (1.39 secs, 4,803,278,104 bytes)
--    λ> sum (sumaVec4 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (0.92 secs, 4,002,161,488 bytes)
--    λ> sum (sumaVec5 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (0.87 secs, 4,001,641,648 bytes)
--    λ> sum (sumaVec6 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (1.42 secs, 4,802,881,560 bytes)
--    λ> sum (sumaVec7 (replicate 5000 (replicate 5000 1)))
--    25000000
--    (10.14 secs, 11,203,918,144 bytes)
