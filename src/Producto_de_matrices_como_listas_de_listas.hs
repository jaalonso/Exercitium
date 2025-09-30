-- Producto_de_matrices_como_listas_de_listas.hs
-- Producto de matrices como listas de listas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-Julio-2014 (actualizado 30-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices pueden representarse mediante una lista de listas donde
-- cada una de las lista representa una fila  de la matriz. Por ejemplo,
-- la matriz
--    |1 0 -2|
--    |0 3 -1|
-- puede representarse por [[1,0,-2],[0,3,-1]].
--
-- Definir la función
--    producto :: Num a => [[a]] -> [[a]] -> [[a]]
-- tal que (producto p q) es el producto de las matrices p y q. Por
-- ejemplo,
--    λ> producto [[1,0,-2],[0,3,-1]] [[0,3],[-2,-1],[0,4]]
--    [[0,-5],[-6,-7]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Producto_de_matrices_como_listas_de_listas where

import Data.List (transpose)
import Data.Matrix (fromLists, toLists)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

producto1 :: Num a => [[a]] -> [[a]] -> [[a]]
producto1 p q =
  [[sum [x*y | (x,y) <- zip fila col] | col <- cols] | fila <- p]
  where
    cols = transpose q

-- 2ª solución
-- ===========

producto2 :: Num a => [[a]] -> [[a]] -> [[a]]
producto2 p q =
  [[sum (zipWith (*) fila col) | col <- cols] | fila <- p]
  where
    cols = transpose q

-- 3ª solución
-- ===========

producto3 :: Num a => [[a]] -> [[a]] -> [[a]]
producto3 [] _ = []
producto3 (fila:filas) q = map (productoEscalar fila) cols : producto3 filas q
  where
    cols = transpose q

productoEscalar :: Num a => [a] -> [a] -> a
productoEscalar (x:xs) (y:ys) = x * y + productoEscalar xs ys
productoEscalar _      _      = 0

-- 4ª solución
-- ===========

producto4 :: Num a => [[a]] -> [[a]] -> [[a]]
producto4 p q = map (\fila -> map (productoEscalar fila) (transpose q)) p

-- 5ª solución
-- ===========

producto5 :: Num a => [[a]] -> [[a]] -> [[a]]
producto5 p q =
  toLists (fromLists p * fromLists q)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Integer]] -> [[Integer]] -> [[Integer]]) -> Spec
specG producto = do
  it "e1" $
    producto [[1,0,-2],[0,3,-1]] [[0,3],[-2,-1],[0,4]]
    `shouldBe` [[0,-5],[-6,-7]]

spec :: Spec
spec = do
  describe "def. 1" $ specG producto1
  describe "def. 2" $ specG producto2
  describe "def. 3" $ specG producto3
  describe "def. 4" $ specG producto4
  describe "def. 5" $ specG producto5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Generador de matrices compatibles
genMatrices :: (Num a, Arbitrary a) => Gen ( [[a]], [[a]] )
genMatrices = do
  i <- choose (1, 5)
  j <- choose (1, 5)
  k <- choose (1, 5)
  p <- vectorOf i (vectorOf j arbitrary)
  q <- vectorOf j (vectorOf k arbitrary)
  return (p, q)

-- La propiedad es
prop_producto :: Property
prop_producto =
  forAll (genMatrices :: Gen ([[Integer]], [[Integer]])) $ \(p, q) ->
  all (== producto1 p q)
      [producto2 p q,
       producto3 p q,
       producto4 p q,
       producto5 p q]

-- La comprobación es
--    λ> quickCheck prop_producto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ej = replicate 2000 [1..2000]
--    λ> last (last (producto1 ej ej))
--    4002000000
--    (0.23 secs, 774,699,016 bytes)
--    λ> last (last (producto2 ej ej))
--    4002000000
--    (0.20 secs, 774,490,928 bytes)
--    λ> last (last (producto3 ej ej))
--    4002000000
--    (0.20 secs, 774,909,856 bytes)
--    λ> last (last (producto4 ej ej))
--    4002000000
--    (0.20 secs, 774,588,984 bytes)
--    λ> last (last (producto5 ej ej))
--    4002000000
--    (5.18 secs, 4,431,551,416 bytes)
