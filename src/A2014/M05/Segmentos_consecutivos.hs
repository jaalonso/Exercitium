-- Segmentos_consecutivos.hs
-- Segmentos de elementos consecutivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    segmentos :: (Enum a, Eq a) => [a] -> [[a]]
-- tal que (segmentos xss) es la lista de los segmentos de xss formados
-- por elementos consecutivos. Por ejemplo,
--    segmentos [1,2,5,6,4]     ==  [[1,2],[5,6],[4]]
--    segmentos [1,2,3,4,7,8,9] ==  [[1,2,3,4],[7,8,9]]
--    segmentos "abbccddeeebc"  ==  ["ab","bc","cd","de","e","e","bc"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module A2014.M05.Segmentos_consecutivos where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

segmentos1 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos1 []  = []
segmentos1 xs = ys : segmentos1 zs
  where ys = inicial xs
        n  = length ys
        zs = drop n xs

-- (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
--    inicial [1,2,5,6,4]    ==  [1,2]
--    inicial "abccddeeebc"  ==  "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial []      = []
inicial [x]     = [x]
inicial (x:y:xs)
  | succ x == y = x : inicial (y:xs)
  | otherwise   = [x]

-- 2ª solución
-- ===========

segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
  where (ys,zs) = inicialYresto xs

-- (inicialYresto xs) es par formado por el segmento inicial de xs
-- con elementos consecutivos junto con los restantes elementos. Por
-- ejemplo,
--    inicialYresto [1,2,5,6,4]    ==  ([1,2],[5,6,4])
--    inicialYresto "abccddeeebc"  ==  ("abc","cddeeebc")
inicialYresto :: (Enum a, Eq a) => [a] -> ([a],[a])
inicialYresto []      = ([],[])
inicialYresto [x]     = ([x],[])
inicialYresto (x:y:xs)
  | succ x == y = (x:us,vs)
  | otherwise   = ([x],y:xs)
  where (us,vs) = inicialYresto (y:xs)

-- 3ª solución
-- ===========

segmentos3 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos3 []  = []
segmentos3 [x] = [[x]]
segmentos3 (x:xs) | y == succ x = (x:y:ys):zs
                  | otherwise   = [x] : (y:ys):zs
  where ((y:ys):zs) = segmentos3 xs

-- 4ª solución
-- ===========

segmentos4 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos4 []  = []
segmentos4 xs = ys : segmentos4 zs
  where ys = inicial4 xs
        n  = length ys
        zs = drop n xs

inicial4 :: (Enum a, Eq a) => [a] -> [a]
inicial4 [] = []
inicial4 (x:xs) =
  map fst (takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..]))

-- 5ª solución
-- ===========

segmentos5 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos5 []  = []
segmentos5 xs = ys : segmentos5 zs
  where ys = inicial5 xs
        n  = length ys
        zs = drop n xs

inicial5 :: (Enum a, Eq a) => [a] -> [a]
inicial5 [] = []
inicial5 (x:xs) =
  map fst (takeWhile (uncurry (==)) (zip (x:xs) [x..]))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [[Int]]) -> Spec
specG segmentos = do
  it "e1" $
    segmentos [1,2,5,6,4]     `shouldBe`  [[1,2],[5,6],[4]]
  it "e2" $
    segmentos [1,2,3,4,7,8,9] `shouldBe`  [[1,2,3,4],[7,8,9]]

spec :: Spec
spec = do
  describe "def. 1" $ specG segmentos1
  describe "def. 2" $ specG segmentos2
  describe "def. 3" $ specG segmentos3
  describe "def. 4" $ specG segmentos4
  describe "def. 5" $ specG segmentos5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_segmentos :: [Int] -> Bool
prop_segmentos xs =
  all (== segmentos1 xs)
      [segmentos2 xs,
       segmentos3 xs,
       segmentos4 xs,
       segmentos5 xs]

-- La comprobación es
--    λ> quickCheck prop_segmentos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (segmentos1 (take (10^6) (cycle [1..10^3])))
--    1000
--    (0.69 secs, 416,742,208 bytes)
--    λ> length (segmentos2 (take (10^6) (cycle [1..10^3])))
--    1000
--    (0.66 secs, 528,861,976 bytes)
--    λ> length (segmentos3 (take (10^6) (cycle [1..10^3])))
--    1000
--    (2.35 secs, 1,016,276,896 bytes)
--    λ> length (segmentos4 (take (10^6) (cycle [1..10^3])))
--    1000
--    (0.27 secs, 409,438,368 bytes)
--    λ> length (segmentos5 (take (10^6) (cycle [1..10^3])))
--    1000
--    (0.13 secs, 401,510,360 bytes)
--
--    λ> length (segmentos4 (take (10^7) (cycle [1..10^3])))
--    10000
--    (2.35 secs, 4,088,926,920 bytes)
--    λ> length (segmentos5 (take (10^7) (cycle [1..10^3])))
--    10000
--    (1.02 secs, 4,009,646,928 bytes)
