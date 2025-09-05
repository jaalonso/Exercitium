-- Elimina_n_elementos.hs
-- Eliminación de n elementos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-Junio-2014 (actualizado 5-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    elimina :: Int -> [a] -> [[a]]
-- tal que (elimina n xs) es la lista de las listas obtenidas eliminando
-- n elementos de xs. Por ejemplo,
--    elimina 0 "abcd"  ==  ["abcd"]
--    elimina 1 "abcd"  ==  ["bcd","acd","abd","abc"]
--    elimina 2 "abcd"  ==  ["cd","bd","bc","ad","ac","ab"]
--    elimina 3 "abcd"  ==  ["d","c","b","a"]
--    elimina 4 "abcd"  ==  [""]
--    elimina 5 "abcd"  ==  []
--    elimina 6 "abcd"  ==  []
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elimina_n_elementos where

import Data.List (sort, subsequences)
import Math.Combinat.Sets (choose)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

elimina1 :: Int -> [a] -> [[a]]
elimina1 0 xs     = [xs]
elimina1 _ []     = []
elimina1 n (x:xs) = elimina1 (n-1) xs ++ [x:ys | ys <- elimina1 n xs]

-- 2ª solución
-- ===========

elimina2 :: Int -> [a] -> [[a]]
elimina2 0 xs     = [xs]
elimina2 _ []     = []
elimina2 n (x:xs) = elimina2 (n-1) xs ++ map (x:) (elimina2 n xs)

-- 3ª solución
-- ===========

elimina3 :: Int -> [a] -> [[a]]
elimina3 n xs =
  reverse [ys | ys <- subsequences xs, length ys == k]
  where k = length xs - n

-- 4ª solución
-- ===========

elimina4 :: Int -> [a] -> [[a]]
elimina4 n xs = combinaciones (length xs - n) xs

-- (combinaciones k xs) es la lista de las combinaciones de orden k de
-- los elementos de la lista xs. Por ejemplo,
--    λ> combinaciones 2 "bcde"
--    ["bc","bd","be","cd","ce","de"]
--    λ> combinaciones 3 "bcde"
--    ["bcd","bce","bde","cde"]
--    λ> combinaciones 3 "abcde"
--    ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
combinaciones :: Int -> [a] -> [[a]]
combinaciones 0 _          = [[]]
combinaciones _ []         = []
combinaciones k (x:xs) =
  [x:ys | ys <- combinaciones (k-1) xs] ++ combinaciones k xs

-- 5ª solución
-- ===========

elimina5 :: Int -> [a] -> [[a]]
elimina5 n xs = combinaciones2 (length xs - n) xs

combinaciones2 :: Int -> [a] -> [[a]]
combinaciones2 0 _      = [[]]
combinaciones2 _ []     = []
combinaciones2 k (x:xs) =
  map (x:) (combinaciones2 (k-1) xs) ++ combinaciones2 k xs

-- 6ª solución
-- ===========

elimina6 :: Int -> [a] -> [[a]]
elimina6 n xs
  | n < 0 || n > length xs = []
  | otherwise = selecciona (length xs - n) xs
  where
    selecciona 0 _      = [[]]
    selecciona _ []     = []
    selecciona k (y:ys) = map (y:) (selecciona (k-1) ys) ++ selecciona k ys

-- 7ª solución
-- ===========

elimina7 :: Int -> [a] -> [[a]]
elimina7 n xs = choose (length xs - n) xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> String -> [String]) -> Spec
specG elimina = do
  it "e1" $
    elimina' 0 "abcd"  `shouldBe` ["abcd"]
  it "e2" $
    elimina' 1 "abcd"  `shouldBe` ["abc","abd","acd","bcd"]
  it "e3" $
    elimina' 2 "abcd"  `shouldBe` ["ab","ac","ad","bc","bd","cd"]
  it "e4" $
    elimina' 3 "abcd"  `shouldBe` ["a","b","c","d"]
  it "e5" $
    elimina' 4 "abcd"  `shouldBe` [""]
  it "e6" $
    elimina' 5 "abcd"  `shouldBe` []
  it "e7" $
    elimina' 6 "abcd"  `shouldBe` []
  where elimina' n xs = sort (elimina n xs)

spec :: Spec
spec = do
  describe "def. 1"  $ specG elimina1
  describe "def. 2"  $ specG elimina2
  describe "def. 3"  $ specG elimina3
  describe "def. 4"  $ specG elimina4
  describe "def. 5"  $ specG elimina5
  describe "def. 6"  $ specG elimina6
  describe "def. 7"  $ specG elimina7

-- La verificación es
--    λ> verifica
--    49 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_elimina :: Int -> [Int] -> Bool
prop_elimina n xs =
  all (`igual` elimina1 n' xs')
      [elimina2 n' xs',
       elimina3 n' xs',
       elimina4 n' xs',
       elimina5 n' xs',
       elimina6 n' xs',
       elimina7 n' xs']
  where igual as bs = sort as == sort bs
        n' = n `mod` 3
        xs' = take 10 xs

-- La comprobación es
--    λ> quickCheck prop_intercala
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (elimina1 3 [1..21])
--    1330
--    (0.01 secs, 3,682,200 bytes)
--    λ> length (elimina2 3 [1..21])
--    1330
--    (0.02 secs, 3,082,664 bytes)
--    λ> length (elimina3 3 [1..21])
--    1330
--    (0.79 secs, 436,934,736 bytes)
--    λ> length (elimina4 3 [1..21])
--    1330
--    (1.69 secs, 893,960,152 bytes)
--    λ> length (elimina5 3 [1..21])
--    1330
--    (1.62 secs, 859,643,368 bytes)
--    λ> length (elimina6 3 [1..21])
--    1330
--    (2.72 secs, 1,329,353,632 bytes)
--    λ> length (elimina7 3 [1..21])
--    1330
--    (0.07 secs, 119,942,472 bytes)
--
--    λ> length (elimina1 3 [1..27])
--    2925
--    (0.01 secs, 8,903,640 bytes)
--    λ> length (elimina2 3 [1..27])
--    2925
--    (0.03 secs, 7,165,992 bytes)
--    λ> length (elimina7 3 [1..27])
--    2925
--    (2.16 secs, 7,522,387,728 bytes)
--
--    λ> length (elimina1 3 [1..150])
--    551300
--    (10.12 secs, 7,643,122,544 bytes)
--    λ> length (elimina2 3 [1..150])
--    551300
--    (4.31 secs, 5,689,134,120 bytes)
