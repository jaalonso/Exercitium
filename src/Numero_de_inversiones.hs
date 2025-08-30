-- Numero_de_inversiones.hs
-- Número de inversiones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-Junio-2014 (actualiado 30-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se dice que en una sucesión de números x(1), x(2), ..., x(n) hay una
-- inversión cuando existe un par de números x(i) > x(j), siendo i < j.
-- Por ejemplo, en la permutación 2, 1, 4, 3 hay dos inversiones
-- (2 antes que 1 y 4 antes que 3) y en la permutación 4, 3, 1, 2 hay
-- cinco inversiones (4 antes 3, 4 antes 1, 4 antes 2, 3 antes 1,
-- 3 antes 2).
--
-- Definir la función
--    numeroInversiones :: Ord a => [a] -> Int
-- tal que (numeroInversiones xs) es el número de inversiones de xs. Por
-- ejemplo,
--    numeroInversiones [2,1,4,3]  ==  2
--    numeroInversiones [4,3,1,2]  ==  5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numero_de_inversiones where

import Test.QuickCheck (quickCheck)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Data.Array ((!), listArray)

-- 1ª solución
-- ===========

numeroInversiones1 :: Ord a => [a] -> Int
numeroInversiones1 = length . indicesInversiones

-- (indicesInversiones xs) es la lista de los índices de las inversiones
-- de xs. Por ejemplo,
--    indicesInversiones [2,1,4,3]  ==  [(0,1),(2,3)]
--    indicesInversiones [4,3,1,2]  ==  [(0,1),(0,2),(0,3),(1,2),(1,3)]
indicesInversiones :: Ord a => [a] -> [(Int,Int)]
indicesInversiones xs = [(i,j) | i <- [0..n-2],
                                 j <- [i+1..n-1],
                                 xs!!i > xs!!j]
  where n = length xs

-- 2ª solución
-- ===========

numeroInversiones2 :: Ord a => [a] -> Int
numeroInversiones2 = length . indicesInversiones2

indicesInversiones2 :: Ord a => [a] -> [(Int,Int)]
indicesInversiones2 xs = [(i,j) | i <- [0..n-2],
                                  j <- [i+1..n-1],
                                  v!i > v!j]
  where n = length xs
        v = listArray (0,n-1) xs

-- 3ª solución
-- ===========

numeroInversiones3 :: Ord a => [a] -> Int
numeroInversiones3 = length . inversiones

-- (inversiones xs) es la lista de las inversiones  de xs. Por ejemplo,
--    Inversiones [2,1,4,3]  ==  [(2,1),(4,3)]
--    Inversiones [4,3,1,2]  ==  [(4,3),(4,1),(4,2),(3,1),(3,2)]
inversiones :: Ord a => [a] -> [(a,a)]
inversiones []     = []
inversiones (x:xs) = [(x,y) | y <- xs, y < x] ++ inversiones xs

-- 4ª solución
-- ===========

numeroInversiones4 :: Ord a => [a] -> Int
numeroInversiones4 []     = 0
numeroInversiones4 (x:xs) = length (filter (x>) xs) + numeroInversiones4 xs

-- 5ª solución
-- ===========

numeroInversiones5 :: Ord a => [a] -> Int
numeroInversiones5 xs = snd (ordenadaConInversiones xs)

ordenadaConInversiones :: Ord a => [a] -> ([a], Int)
ordenadaConInversiones []  = ([], 0)
ordenadaConInversiones [x] = ([x], 0)
ordenadaConInversiones xs  =
  (mezcla, izqInversiones + dchaInversiones + mezclaInversiones)
  where
  (izq, dcha) = splitAt (length xs `div` 2) xs
  (izqOrdenada, izqInversiones)  = ordenadaConInversiones izq
  (dchaOrdenada, dchaInversiones) = ordenadaConInversiones dcha
  (mezcla, mezclaInversiones) = mezclaYcuenta izqOrdenada dchaOrdenada

mezclaYcuenta :: Ord a => [a] -> [a] -> ([a], Int)
mezclaYcuenta [] ys = (ys, 0)
mezclaYcuenta xs [] = (xs, 0)
mezclaYcuenta (x:xs) (y:ys)
  | x <= y = let (zs, n) = mezclaYcuenta xs (y:ys) in
      (x : zs, n)
  | otherwise = let (zs, n) = mezclaYcuenta (x:xs) ys in
      (y : zs, length xs + n + 1)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> Int) -> Spec
specG numeroInversiones = do
  it "e1" $
    numeroInversiones [2,1,4,3]  `shouldBe`  2
  it "e2" $
    numeroInversiones [4,3,1,2]  `shouldBe`  5

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroInversiones1
  describe "def. 2" $ specG numeroInversiones2
  describe "def. 3" $ specG numeroInversiones3
  describe "def. 4" $ specG numeroInversiones4
  describe "def. 5" $ specG numeroInversiones5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numeroInversiones :: [Int] -> Bool
prop_numeroInversiones xs =
  all (== numeroInversiones1 xs)
      [numeroInversiones2 xs,
       numeroInversiones3 xs,
       numeroInversiones4 xs,
       numeroInversiones5 xs]

-- La comprobación es
--    λ> quickCheck prop_numeroInversiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numeroInversiones1 [1200,1199..1]
--    719400
--    (2.15 secs, 260,102,328 bytes)
--    λ> numeroInversiones2 [1200,1199..1]
--    719400
--    (0.36 secs, 294,624,048 bytes)
--    λ> numeroInversiones3 [1200,1199..1]
--    719400
--    (0.21 secs, 150,647,848 bytes)
--    λ> numeroInversiones4 [1200,1199..1]
--    719400
--    (0.06 secs, 41,504,368 bytes)
--    λ> numeroInversiones5 [1200,1199..1]
--    719400
--    (0.06 secs, 6,825,296 bytes)
--    λ> numeroInversiones3 [3000,2999..1]
--    4498500
--    (1.03 secs, 937,320,624 bytes)
--    λ> numeroInversiones4 [3000,2999..1]
--    4498500
--    (0.40 secs, 254,111,416 bytes)
--    λ> numeroInversiones5 [3000,2999..1]
--    4498500
--    (0.09 secs, 17,593,416 bytes)
