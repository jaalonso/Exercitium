-- Coeficientes_binomiales.hs
-- Coeficientes binomiales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-septiembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El coeficiente binomial n sobre k es el número de subconjuntos de k
-- elementos escogidos de un conjunto con n elementos.
--
-- Definir la función
--    binomial :: Integer -> Integer -> Integer
-- tal que (binomial n k) es el coeficiente binomial n sobre k. Por
-- ejemplo,
--    binomial 6 3 == 20
--    binomial 5 2 == 10
--    binomial 5 3 == 10
-- ---------------------------------------------------------------------

module Coeficientes_binomiales where

import Data.Array (Array, (!), array)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición (por recursión)
-- =============================

binomial1 :: Integer -> Integer -> Integer
binomial1 _ 0 = 1
binomial1 n k
  | n == k    = 1
  | otherwise = binomial1 (n-1) (k-1) + binomial1 (n-1) k

-- 2ª definición (con programación dinámica)
-- =========================================

binomial2 :: Integer -> Integer -> Integer
binomial2 n k = matrizBinomial2 n k ! (n,k)

-- (matrizBinomial2 n k) es la matriz de orden (n+1)x(k+1) tal que el
-- valor en la posición (i,j) (con j <= i) es el coeficiente binomial i
-- sobre j. Por ejemplo,
--    λ> [[(matrizBinomial2 3 3)!(i,j) | j <- [0..i]] | i <- [0..3]]
--    [[1],[1,1],[1,2,1],[1,3,3,1]]
matrizBinomial2 :: Integer -> Integer -> Array (Integer,Integer) Integer
matrizBinomial2 n k = q where
  q = array ((0,0),(n,k)) [((i,j),f i j) | i <- [0..n], j <- [0..k]]
  f _ 0 = 1
  f i j
    | i == j    = 1
    | otherwise = q!(i-1,j-1) + q!(i-1,j)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> binomial1 25 12
--    5200300
--    (6.45 secs, 2,654,797,776 bytes)
--    λ> binomial2 25 12
--    5200300
--    (0.00 secs, 826,272 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    binomial1 6 3 `shouldBe` 20
  it "e2" $
    binomial1 5 2 `shouldBe` 10
  it "e3" $
    binomial1 5 3 `shouldBe` 10
  it "e4" $
    binomial2 6 3 `shouldBe` 20
  it "e5" $
    binomial2 5 2 `shouldBe` 10
  it "e6" $
    binomial2 5 3 `shouldBe` 10

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--
--    Finished in 0.0006 seconds
--    6 examples, 0 failures
