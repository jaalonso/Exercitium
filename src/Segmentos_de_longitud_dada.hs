-- Segmentos_de_longitud_dada.hs
-- Segmentos de longitud dada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-Febrero-2015 (actualizado 2-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    segmentos :: Int -> [a] -> [[a]]
-- tal que (segmentos n xs) es la lista de los segmentos de longitud n
-- de la lista xs. Por ejemplo,
--    segmentos 3 [1..5]  ==  [[1,2,3],[2,3,4],[3,4,5]]
-- ---------------------------------------------------------------------

module Segmentos_de_longitud_dada where

import Data.List (transpose, tails)
import Data.List.Split (divvy)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Recursión simple
-- ==============================

segmentos1 :: Int -> [a] -> [[a]]
segmentos1 n xs
  | length xs < n = []
  | otherwise     = take n xs : segmentos1 n (tail xs)

-- 2ª solución: Comprensión de listas con índices
-- ==============================================

segmentos2 :: Int -> [a] -> [[a]]
segmentos2 n xs =
  [take n (drop i xs) | i <- [0..length xs - n]]

-- 3ª solución: Comprensión de listas con tails
-- ============================================

segmentos3 :: Int -> [a] -> [[a]]
segmentos3 n xs =
  [take n t | t <- tails xs, length t >= n]

-- 4ª solución: Composición funcional con tails
-- ============================================

segmentos4 :: Int -> [a] -> [[a]]
segmentos4 n xs =
  takeWhile (\ys -> length ys == n) (map (take n) (tails xs))

-- 5ª solución: Optimización precalculando el número de segmentos
-- ==============================================================

segmentos5 :: Int -> [a] -> [[a]]
segmentos5 n xs =
  take (length xs - n + 1) (map (take n) (tails xs))

-- 6ª solución: Recursión con dos punteros
-- =======================================

segmentos6 :: Int -> [a] -> [[a]]
segmentos6 n xs = aux (drop (n - 1) xs) xs
  where
    aux (_:ts) (y:ys) = take n (y:ys) : aux ts ys
    aux _ _           = []

-- 7ª solución: Usa 'zipWith const' para recortar los segmentos sobrantes
-- ======================================================================

segmentos7 :: Int -> [a] -> [[a]]
segmentos7 n xs =
  zipWith const (map (take n) (tails xs)) (drop (n-1) xs)

-- 8ª solución: Usando transpose
-- =============================

segmentos8 :: Int -> [a] -> [[a]]
segmentos8 n xs =
  takeWhile (\ys -> length ys == n) (transpose (take n (tails xs)))

-- 9ª solución: Uso de la librería split
-- ======================================

segmentos9 :: Int -> [a] -> [[a]]
segmentos9 = flip divvy 1

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> [[Int]]) -> Spec
specG segmentos = do
  it "e1" $
    segmentos 3 [1..5] `shouldBe` [[1,2,3],[2,3,4],[3,4,5]]

spec :: Spec
spec = do
  describe "def. 1" $ specG segmentos1
  describe "def. 2" $ specG segmentos2
  describe "def. 3" $ specG segmentos3
  describe "def. 4" $ specG segmentos4
  describe "def. 5" $ specG segmentos5
  describe "def. 6" $ specG segmentos6
  describe "def. 7" $ specG segmentos7
  describe "def. 8" $ specG segmentos8
  describe "def. 9" $ specG segmentos9

-- La verificación es
--    λ> verifica
--    9 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> [Int] -> Bool
prop_equivalencia (Positive n) xs =
  all (== segmentos1 n xs)
      [ segmentos2 n xs
      , segmentos3 n xs
      , segmentos4 n xs
      , segmentos5 n xs
      , segmentos6 n xs
      , segmentos7 n xs
      , segmentos8 n xs
      , segmentos9 n xs
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (segmentos1 3 [1..30000])
--    29998
--    (2.37 secs, 12,134,184 bytes)
--    λ> length (segmentos2 3 [1..30000])
--    29998
--    (0.04 secs, 8,773,112 bytes)
--    λ> length (segmentos3 3 [1..30000])
--    29998
--    (2.46 secs, 10,933,280 bytes)
--    λ> length (segmentos4 3 [1..30000])
--    29998
--    (0.04 secs, 14,293,000 bytes)
--    λ> length (segmentos5 3 [1..30000])
--    29998
--    (0.02 secs, 8,533,184 bytes)
--    λ> length (segmentos6 3 [1..30000])
--    29998
--    (0.04 secs, 9,013,120 bytes)
--    λ> length (segmentos7 3 [1..30000])
--    29998
--    (0.02 secs, 9,973,176 bytes)
--    λ> length (segmentos8 3 [1..30000])
--    29998
--    (0.05 secs, 19,332,992 bytes)
--    λ> length (segmentos9 3 [1..30000])
--    29998
--    (0.02 secs, 14,773,224 bytes)
--
--    λ> length (segmentos2 3 [1..6000000])
--    5999998
--    (2.08 secs, 1,632,614,640 bytes)
--    λ> length (segmentos4 3 [1..6000000])
--    5999998
--    (2.07 secs, 2,736,614,520 bytes)
--    λ> length (segmentos5 3 [1..6000000])
--    5999998
--    (1.09 secs, 1,584,614,704 bytes)
--    λ> length (segmentos6 3 [1..6000000])
--    5999998
--    (2.09 secs, 1,680,614,640 bytes)
--    λ> length (segmentos7 3 [1..6000000])
--    5999998
--    (0.38 secs, 1,872,614,696 bytes)
--    λ> length (segmentos8 3 [1..6000000])
--    5999998
--    (2.30 secs, 3,744,614,512 bytes)
--    λ> length (segmentos9 3 [1..6000000])
--    5999998
--    (0.66 secs, 2,832,614,688 bytes)
--
--    λ> length (segmentos7 3 [1..30000000])
--    29999998
--    (1.77 secs, 9,360,615,456 bytes)
--    λ> length (segmentos9 3 [1..30000000])
--    29999998
--    (3.17 secs, 14,160,615,448 bytes)
