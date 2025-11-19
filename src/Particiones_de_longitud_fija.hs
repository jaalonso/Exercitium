-- Particiones_de_longitud_fija.hs
-- Particiones de longitud fija.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-Noviembre-2014 (actualizado 19-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    particionesFijas :: Int -> Int -> [[Int]]
-- tal que (particionesFijas n k) es la lista de listas de k números
-- enteros positivos cuya suma es el numero entero positivo n. Por ejemplo,
--    λ> particionesFijas 8 2
--    [[4,4],[5,3],[6,2],[7,1]]
--    λ> particionesFijas 8 3
--    [[3,3,2],[4,2,2],[4,3,1],[5,2,1],[6,1,1]]
--    λ> particionesFijas 9 3
--    [[3,3,3],[4,3,2],[4,4,1],[5,2,2],[5,3,1],[6,2,1],[7,1,1]]
--    λ> length (particionesFijas 67 5)
--    8056
-- ---------------------------------------------------------------------

module Particiones_de_longitud_fija where

import Data.List (sort)
import Math.Combinat.Partitions.Integer (partitionsWithKParts, fromPartition)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

particionesFijas1 :: Int -> Int -> [[Int]]
particionesFijas1 n 1 = [[n]]
particionesFijas1 n k
  | k < 1     = []
  | otherwise = [x:y:ys | x <- [1..n-1],
                          (y:ys) <- particionesFijas1 (n-x) (k-1),
                          x >= y]

-- 2ª solución
-- ===========

particionesFijas2 :: Int -> Int -> [[Int]]
particionesFijas2 n k
  | k <= 0    = []
  | k == 1    = [[n]]
  | k == n    = [replicate k 1]
  | k > n     = []
  | otherwise = [xs ++ [1] | xs <- particionesFijas2 (n-1) (k-1)] ++
                [[x+1 | x <- xs] | xs <- particionesFijas2 (n-k) k]

-- 3ª solución
-- ===========

particionesFijas3 :: Int -> Int -> [[Int]]
particionesFijas3 n k = aux n k n
  where
    aux 0 0 _ = [[]]
    aux _ 0 _ = []
    aux m j tope =
      [x : xs | x <- [minimo .. maximo]
              , xs <- aux (m - x) (j - 1) x]
      where
        minimo = (m + j - 1) `div` j
        maximo = min tope (m - j + 1)

-- 4ª solución
-- ===========

particionesFijas4 :: Int -> Int -> [[Int]]
particionesFijas4 n k =
  map fromPartition (partitionsWithKParts k n)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Int -> [[Int]]) -> Spec
specG particionesFijas = do
  it "e1" $
    sort (particionesFijas 8 3) `shouldBe`
    [[3,3,2],[4,2,2],[4,3,1],[5,2,1],[6,1,1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG particionesFijas1
  describe "def. 2" $ specG particionesFijas2
  describe "def. 3" $ specG particionesFijas3
  describe "def. 4" $ specG particionesFijas4

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Positive Int -> Bool
prop_equivalencia (Positive n) (Positive k) =
  all (== sort (particionesFijas1 n k))
      [sort (particionesFijas2 n k),
       sort (particionesFijas3 n k),
       sort (particionesFijas4 n k)]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (particionesFijas1 1000 3)
--    83333
--    (0.48 secs, 325,041,680 bytes)
--    λ> length (particionesFijas2 1000 3)
--    83333
--    (1.95 secs, 2,691,333,168 bytes)
--    λ> length (particionesFijas3 1000 3)
--    83333
--    (0.34 secs, 214,234,304 bytes)
--    λ> length (particionesFijas4 1000 3)
--    83333
--    (0.04 secs, 49,345,280 bytes)
--
--    λ> length (particionesFijas1 300 4)
--    189375
--    (3.88 secs, 3,011,903,064 bytes)
--    λ> length (particionesFijas2 300 4)
--    189375
--    (2.40 secs, 1,975,362,504 bytes)
--    λ> length (particionesFijas3 300 4)
--    189375
--    (0.74 secs, 523,098,752 bytes)
--    λ> length (particionesFijas4 300 4)
--    189375
--    (0.07 secs, 146,322,072 bytes)
--
--    λ> length (particionesFijas1 100 5)
--    38225
--    (3.55 secs, 2,694,392,288 bytes)
--    λ> length (particionesFijas2 100 5)
--    38225
--    (0.22 secs, 168,220,744 bytes)
--    λ> length (particionesFijas3 100 5)
--    38225
--    (0.21 secs, 124,942,088 bytes)
--    λ> length (particionesFijas4 100 5)
--    38225
--    (0.01 secs, 38,715,752 bytes)
