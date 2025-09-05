-- Intercala_n_copias.hs
-- Intercalación de n copias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-Junio-2014 (actualizado 5-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    intercala :: Int -> a -> [a] -> [[a]]
-- tal que (intercala n x ys) es la lista de la listas obtenidas
-- intercalando n copias de x en ys, suponiendo que x no pertenece a
-- ys. Por ejemplo,
--    intercala 2 'a' "bc" == ["bcaa","baca","baac","abca","abac","aabc"]
--    intercala 2 'a' "c"  == ["caa","aca","aac"]
--    intercala 1 'a' "c"  == ["ca","ac"]
--    intercala 0 'a' "c"  == ["c"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Intercala_n_copias where

import Data.List (nub, sort)
import Data.Set (fromList, singleton, toList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

intercala1 :: Int -> a -> [a] -> [[a]]
intercala1 0 _ xs = [xs]
intercala1 n y [] = [replicate n y]
intercala1 n y (x:xs) =
  concat [[replicate i y ++ x : zs | zs <- intercala1 (n-i) y xs]
          | i <- [0..n]]

-- 2ª solución
-- ===========

intercala2 :: Int -> a -> [a] -> [[a]]
intercala2 0 _ xs = [xs]
intercala2 n y [] = [replicate n y]
intercala2 n y (x:xs) =
  concatMap (\i -> [replicate i y ++ x : zs | zs <- intercala1 (n-i) y xs]) [0..n]

-- 3ª solución
-- ===========

intercala3 :: Int -> a -> [a] -> [[a]]
intercala3 0 _ ys = [ys]
intercala3 n x ys = aux n ys []
  where
    aux 0 ys' zs = [zs ++ ys']
    aux n' [] zs = [zs ++ replicate n' x]
    aux n' (y:ys') zs =
      aux n' ys' (zs ++ [y]) ++
      aux (n'-1) (y:ys') (zs ++ [x])

-- 4ª solución
-- ===========

intercala4 :: Eq a => Int -> a -> [a] -> [[a]]
intercala4 n x ys = nub (aux n ys)
  where
    aux 0 ys' = [ys']
    aux n' ys' = concat [intercalaUno x zs | zs <- aux (n'-1) ys']

-- (intercalaUno x ys) es la lista de las listas obtenidas intercalando
-- una copia de x en ys. Por ejemplo,
--    intercalaUno 'a' "bc"  == ["abc","bac","bca"]
intercalaUno :: a -> [a] -> [[a]]
intercalaUno x []     = [[x]]
intercalaUno x (y:ys) = (x:y:ys) : [y:zs | zs <- intercalaUno x ys]

-- 5ª solución
-- ===========

intercala5 :: Ord a => Int -> a -> [a] -> [[a]]
intercala5 n x ys = toList (aux n ys)
  where
    aux 0 ys'  = singleton ys'
    aux n' ys' = fromList (concat [intercalaUno x zs | zs <- toList (aux (n'-1) ys')])

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Char -> String -> [String]) -> Spec
specG intercala = do
  it "e1" $
    intercala' 2 'a' "bc" `shouldBe` ["aabc", "abac", "abca", "baac", "baca", "bcaa"]
  it "e2" $
    intercala' 2 'a' "c"  `shouldBe` ["aac", "aca", "caa"]
  it "e3" $
    intercala' 1 'a' "c"  `shouldBe` ["ac", "ca"]
  it "e4" $
    intercala' 0 'a' "c"  `shouldBe` ["c"]
  where intercala' n x ys = sort (intercala n x ys)

spec :: Spec
spec = do
  describe "def. 1"  $ specG intercala1
  describe "def. 2"  $ specG intercala2
  describe "def. 3"  $ specG intercala3
  describe "def. 4"  $ specG intercala4
  describe "def. 5"  $ specG intercala5

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_intercala :: Int -> Int -> [Int] -> Bool
prop_intercala n x ys =
  sort (intercala1 m x ys') == sort (intercala2 m x ys')
  where m = n `mod` 3
        ys' = filter (/= x) ys

-- La comprobación es
--    λ> quickCheck prop_intercala
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (intercala1 3 'a' ['b'..'z'])
--    3276
--    (0.06 secs, 17,106,368 bytes)
--    λ> length (intercala2 3 'a' ['b'..'z'])
--    3276
--    (0.06 secs, 17,104,248 bytes)
--    λ> length (intercala3 3 'a' ['b'..'z'])
--    3276
--    (0.01 secs, 6,077,080 bytes)
--    λ> length (intercala4 3 'a' ['b'..'z'])
--    3276
--    (1.51 secs, 49,533,208 bytes)
--    λ> length (intercala5 3 'a' ['b'..'z'])
--    3276
--    (0.07 secs, 31,249,936 bytes)
--
--    λ> length (intercala1 5 'a' ['b'..'z'])
--    142506
--    (1.32 secs, 767,853,344 bytes)
--    λ> length (intercala2 5 'a' ['b'..'z'])
--    142506
--    (1.34 secs, 767,852,344 bytes)
--    λ> length (intercala3 5 'a' ['b'..'z'])
--    142506
--    (0.22 secs, 255,971,880 bytes)
--    λ> length (intercala5 5 'a' ['b'..'z'])
--    142506
--    (6.63 secs, 2,684,942,184 bytes)
