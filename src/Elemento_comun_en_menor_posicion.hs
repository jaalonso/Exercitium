-- Elemento_comun_en_menor_posicion.hs
-- Elemento común en la menor posición.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-Noviembre-2014 (actualizado 15-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    elemento :: Eq a => [a] -> [a] -> [a]
-- tal que (elemento xs ys) es la lista formada por el elemento común a
-- xs e ys con la menor posición global (considerando ambas listas). Por
-- ejemplo.
--    elemento [3,7,6,9,8,0] [5,4,2,7,8,6,9]  ==  [7]
--    elemento [3,7,6,9] [9,5,6]              ==  [9]
--    elemento [5,3,6] [7,6,3]                ==  [3]
--    elemento [0,1,3] [3,1]                  ==  [3]
--    elemento [0,3,2] [1,2,3]                ==  [3]
--    elemento [3,7,6,3,8,0] [5,4,9,1,4,2,1]  ==  []
--    elemento [2,3,5] [7,4]                  ==  []
--
-- Nota: Como se observa en el 3ª ejemplo, en el caso de que un elemento
-- x de xs pertenezca a ys y el elemento de ys en la misma posición que
-- x pertenezca a xs, se elige como el de menor posición el de xs.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elemento_comun_en_menor_posicion where

import Data.List (elemIndex, intersect)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

elemento1 :: Eq a => [a] -> [a] -> [a]
elemento1 xs ys
  | null zs   = []
  | m <= n    = [x]
  | otherwise = [y]
  where zs = interseccion xs ys
        (m,x) = head [(m',x') | (m',x') <- zip [0..] xs, x' `elem` ys]
        (n,y) = head [(n',y') | (n',y') <- zip [0..] ys, y' `elem` xs]

-- (interseccion xs ys) es la lista de los elementos comunes de xs e
-- ys. Por ejemplo,
--    interseccion [3,7,6,9] [9,5,6] == [6,9]
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys =
  [x | x <- xs, x `elem` ys]

-- 2ª solución
-- ===========

elemento2 :: Eq a => [a] -> [a] -> [a]
elemento2 xs ys
  | null zs   = []
  | m <= n    = [x]
  | otherwise = [y]
  where zs = xs `intersect` ys
        (m,x) = head [(m',x') | (m',x') <- zip [0..] xs, x' `elem` zs]
        (n,y) = head [(n',y') | (n',y') <- zip [0..] ys, y' `elem` zs]

-- 3ª solución
-- ===========

elemento3 :: Eq a => [a] -> [a] -> [a]
elemento3 xs ys
  | null zs   = []
  | m <= n    = [x]
  | otherwise = [y]
  where zs = xs `intersect` ys
        x = head [x' | x' <- xs, x' `elem` zs]
        y = head [y' | y' <- ys, y' `elem` zs]
        m = fromJust (elemIndex x xs)
        n = fromJust (elemIndex y ys)

-- 4ª solución
-- ===========

elemento4 :: Eq a => [a] -> [a] -> [a]
elemento4 (x:xs) q@(y:ys)
  | x `elem` q  = [x]
  | y `elem` xs = [y]
  | otherwise   = elemento4 xs ys
elemento4 _ _ = []

-- 5ª solución
-- ===========

elemento5 :: Eq a => [a] -> [a] -> [a]
elemento5 [] _ = []
elemento5 (x:xs) ys
  | x `elem` ys = [x]
  | otherwise   = elemento5 ys xs

-- 6ª solución
-- ===========

elemento6 :: Ord a => [a] -> [a] -> [a]
elemento6 xs ys
  | S.null zs'  = []
  | m <= n    = [x]
  | otherwise = [y]
  where xs' = S.fromList xs
        ys' = S.fromList ys
        zs' = xs' `S.intersection` ys'
        (m,x) = head [(m',x') | (m',x') <- zip [0..] xs, x' `S.member` zs']
        (n,y) = head [(n',y') | (n',y') <- zip [0..] ys, y' `S.member` zs']

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG elemento = do
  it "e1" $
    elemento [3,7,6,9,8,0] [5,4,2,7,8,6,9]  `shouldBe`  [7]
  it "e2" $
    elemento [3,7,6,9,8,0] [5,4,9,1,8,0,1]  `shouldBe`  [9]
  it "e3" $
    elemento [3,7,6,3,8,0] [5,4,9,1,4,2,1]  `shouldBe`  []
  it "e4" $
    elemento [0,1,3] [3,1]                  `shouldBe`  [3]
  it "e5" $
    elemento [0,3,2] [1,2,3]                `shouldBe`  [3]

spec :: Spec
spec = do
  describe "def. 1" $ specG elemento1
  describe "def. 2" $ specG elemento2
  describe "def. 3" $ specG elemento3
  describe "def. 4" $ specG elemento4
  describe "def. 5" $ specG elemento5
  describe "def. 6" $ specG elemento6

-- La verificación es
--    λ> verifica
--    30 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [Int] -> [Int] -> Bool
prop_equivalencia xs ys =
  all (== elemento1 xs ys)
      [elemento2 xs ys,
       elemento3 xs ys,
       elemento4 xs ys,
       elemento5 xs ys,
       elemento6 xs ys]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> elemento1 [1..20000] [30000,29999..20000]
--    [20000]
--    (4.59 secs, 10,203,952 bytes)
--    λ> elemento2 [1..20000] [30000,29999..20000]
--    [20000]
--    (3.27 secs, 10,362,384 bytes)
--    λ> elemento3 [1..20000] [30000,29999..20000]
--    [20000]
--    (3.23 secs, 5,802,840 bytes)
--    λ> elemento4 [1..20000] [30000,29999..20000]
--    [20000]
--    (1.69 secs, 4,601,152 bytes)
--    λ> elemento5 [1..20000] [30000,29999..20000]
--    [20000]
--    (1.68 secs, 5,081,200 bytes)
--    λ> elemento6 [1..20000] [30000,29999..20000]
--    [20000]
--    (0.03 secs, 17,862,792 bytes)
--
--    λ> elemento1 [1..10000] ([20000..30000] ++ [5000])
--    [5000]
--    (1.87 secs, 6,001,976 bytes)
--    λ> elemento2 [1..10000] ([20000..30000] ++ [5000])
--    [5000]
--    (1.78 secs, 6,361,936 bytes)
--    λ> elemento3 [1..10000] ([20000..30000] ++ [5000])
--    [5000]
--    (1.63 secs, 4,082,304 bytes)
--    λ> elemento4 [1..10000] ([20000..30000] ++ [5000])
--    [5000]
--    (0.66 secs, 3,480,280 bytes)
--    λ> elemento5 [1..10000] ([20000..30000] ++ [5000])
--    [5000]
--    (0.62 secs, 3,720,272 bytes)
--    λ> elemento6 [1..10000] ([20000..30000] ++ [5000])
--    [5000]
--    (0.04 secs, 7,169,440 bytes)
--
--    λ> elemento4 [1..2000000] ([3000000..5000000] ++ [1])
--    [1]
--    (0.16 secs, 256,598,232 bytes)
--    λ> elemento5 [1..2000000] ([3000000..5000000] ++ [1])
--    [1]
--    (0.29 secs, 256,598,272 bytes)
--    λ> elemento6 [1..2000000] ([3000000..5000000] ++ [1])
--    [1]
--    (1.77 secs, 1,104,618,904 bytes)
