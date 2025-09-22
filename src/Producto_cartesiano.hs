-- Producto_cartesiano.hs
-- Producto cartesiano de una familia de conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-Julio-2014 (actualizado 22-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos xss.
-- Por ejemplo,
--    λ> producto [[2,5],[6,4]]
--    [[2,6],[2,4],[5,6],[5,4]]
--    λ> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    λ> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    λ> producto []
--    [[]]
--
-- Comprobar con QuickCheck que para toda lista de listas de números
-- enteros, xss, se verifica que el número de elementos de (producto
-- xss) es igual al producto de los números de elementos de cada una de
-- las listas de xss.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Producto_cartesiano where

import Control.Monad (liftM2)
import Control.Applicative (liftA2)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

producto1 :: [[a]] -> [[a]]
producto1 []       = [[]]
producto1 (xs:xss) = [x:ys | x <- xs, ys <- producto1 xss]

-- 2ª solución
-- ===========

producto2 :: [[a]] -> [[a]]
producto2 []       = [[]]
producto2 (xs:xss) = [x:ys | x <- xs, ys <- ps]
  where ps = producto2 xss

-- 3ª solución
-- ===========

producto3 :: [[a]] -> [[a]]
producto3 []       = [[]]
producto3 (xs:xss) = inserta3 xs (producto3 xss)

-- (inserta xs xss) inserta cada elemento de xs en los elementos de
-- xss. Por ejemplo,
--    λ> inserta [1,2] [[3,4],[5,6]]
--    [[1,3,4],[1,5,6],[2,3,4],[2,5,6]]
inserta3 :: [a] -> [[a]] -> [[a]]
inserta3 [] _       = []
inserta3 (x:xs) yss = [x:ys | ys <- yss] ++ inserta3 xs yss

-- 4ª solución
-- ===========

producto4 :: [[a]] -> [[a]]
producto4 = foldr inserta4 [[]]

inserta4 :: [a] -> [[a]] -> [[a]]
inserta4 []     _   = []
inserta4 (x:xs) yss = map (x:) yss ++ inserta4 xs yss

-- 5ª solución
-- ===========

producto5 :: [[a]] -> [[a]]
producto5 = foldr inserta5 [[]]

inserta5 :: [a] -> [[a]] -> [[a]]
inserta5 xs yss = [x:ys | x <- xs, ys <- yss]

-- 6ª solución
-- ===========

producto6 :: [[a]] -> [[a]]
producto6 = foldr inserta6 [[]]

inserta6 :: [a] -> [[a]] -> [[a]]
inserta6 xs yss = concatMap (\x -> map (x:) yss) xs

-- 7ª solución
-- ===========

producto7 :: [[a]] -> [[a]]
producto7 = foldr inserta7 [[]]

inserta7 :: [a] -> [[a]] -> [[a]]
inserta7 xs yss = xs >>= (\x -> map (x:) yss)

-- 8ª solución
-- ===========

producto8 :: [[a]] -> [[a]]
producto8 = foldr inserta8 [[]]

inserta8 :: [a] -> [[a]] -> [[a]]
inserta8 xs yss = (:) <$> xs <*> yss

-- 9ª solución
-- ===========

producto9 :: [[a]] -> [[a]]
producto9 = foldr inserta9 [[]]

inserta9 :: [a] -> [[a]] -> [[a]]
inserta9 = liftA2 (:)

-- 10ª solución
-- ============

producto10 :: [[a]] -> [[a]]
producto10 = foldr (liftM2 (:)) [[]]

-- 11ª solución
-- ============

producto11 :: [[a]] -> [[a]]
producto11 = sequence

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG producto = do
  it "e1" $
    producto [[1,3],[2,5]]
    `shouldBe` [[1,2],[1,5],[3,2],[3,5]]
  it "e2" $
    producto [[1,3],[2,5],[6,4]]
    `shouldBe` [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
  it "e3" $
    producto [[1,3,5],[2,4]]
    `shouldBe` [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]

spec :: Spec
spec = do
  describe "def. 1" $ specG producto1
  describe "def. 2" $ specG producto2
  describe "def. 3" $ specG producto3
  describe "def. 4" $ specG producto4
  describe "def. 5" $ specG producto5
  describe "def. 6" $ specG producto6
  describe "def. 7" $ specG producto7
  describe "def. 8" $ specG producto8
  describe "def. 9" $ specG producto9
  describe "def. 10" $ specG producto10
  describe "def. 11" $ specG producto11

-- La verificación es
--    λ> verifica
--    33 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_producto :: [[Int]] -> Bool
prop_producto xss =
  all (== producto1 xss)
      [ producto2 xss
      , producto3 xss
      , producto4 xss
      , producto5 xss
      , producto6 xss
      , producto7 xss
      , producto8 xss
      , producto9 xss
      , producto10 xss
      , producto11 xss
      ]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize = 9}) prop_producto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (producto1 (replicate 7 [0..9]))
--    10000000
--    (10.04 secs, 10,507,268,856 bytes)
--    λ> length (producto2 (replicate 7 [0..9]))
--    10000000
--    (1.71 secs, 1,333,943,632 bytes)
--    λ> length (producto3 (replicate 7 [0..9]))
--    10000000
--    (2.94 secs, 1,956,176,072 bytes)
--    λ> length (producto4 (replicate 7 [0..9]))
--    10000000
--    (1.06 secs, 1,600,616,296 bytes)
--    λ> length (producto5 (replicate 7 [0..9]))
--    10000000
--    (1.77 secs, 1,333,943,248 bytes)
--    λ> length (producto6 (replicate 7 [0..9]))
--    10000000
--    (1.06 secs, 1,600,608,064 bytes)
--    λ> length (producto7 (replicate 7 [0..9]))
--    10000000
--    (0.34 secs, 1,600,607,784 bytes)
--    λ> length (producto8 (replicate 7 [0..9]))
--    10000000
--    (1.03 secs, 978,390,888 bytes)
--    λ> length (producto9 (replicate 7 [0..9]))
--    10000000
--    (1.20 secs, 1,067,273,920 bytes)
--    λ> length (producto10 (replicate 7 [0..9]))
--    10000000
--    (0.58 secs, 2,311,718,360 bytes)
--    λ> length (producto11 (replicate 7 [0..9]))
--    10000000
--    (1.22 secs, 1,067,273,840 bytes)
--
--    λ> length (producto7 (replicate 7 [1..14]))
--    105413504
--    (3.71 secs, 16,347,812,624 bytes)
--    λ> length (producto10 (replicate 7 [1..14]))
--    105413504
--    (5.12 secs, 23,613,234,792 bytes)
--    λ> length (producto11 (replicate 7 [1..14]))
--    105413504
--    (17.83 secs, 10,898,744,528 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_longitud :: [[Int]] -> Bool
prop_longitud xss =
  length (producto7 xss) == product (map length xss)

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize = 7}) prop_longitud
--    +++ OK, passed 100 tests.
