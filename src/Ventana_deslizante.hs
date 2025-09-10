-- Ventana_deslizante.hs
-- Ventana deslizante.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-Julio-2014 (actualizado 10-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ventanas :: Int -> Int -> [a] -> [[a]]
-- tal que (ventanas x y zs) es la lista de ventanas de zs de tamaño x
-- y deslizamiento y; es decir listas de x elementos consecutivos de zs
-- (salvo, posiblemente, la última que puede ser menor) tales que la
-- diferencia de posiciones entre los primeros elementos de ventanas
-- consecutivas es y. Por ejemplo,
--    ventanas 3 2 [5,1,9,2] == [[5,1,9],[9,2]]
--    ventanas 3 3 [5,1,9,2] == [[5,1,9],[2]]
--    ventanas 3 4 [5,1,9,2] == [[5,1,9]]
--    ventanas 4 1 [1..7]    == [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]
--    ventanas 4 2 [1..7]    == [[1,2,3,4],[3,4,5,6],[5,6,7]]
--    ventanas 4 3 [1..7]    == [[1,2,3,4],[4,5,6,7]]
--    ventanas 4 4 [1..7]    == [[1,2,3,4],[5,6,7]]
--    ventanas 4 5 [1..7]    == [[1,2,3,4],[6,7]]
--    ventanas 4 6 [1..7]    == [[1,2,3,4],[7]]
--    ventanas 4 7 [1..7]    == [[1,2,3,4]]
--    ventanas 4 8 [1..7]    == [[1,2,3,4]]
--    ventanas 3 2 "abcdef"  == ["abc","cde","ef"]
--    ventanas 3 3 "abcdef"  == ["abc","def"]
--    ventanas 3 4 "abcdef"  == ["abc","ef"]
--    ventanas 3 5 "abcdef"  == ["abc","f"]
--    ventanas 3 6 "abcdef"  == ["abc"]
--    ventanas 3 7 "abcdef"  == ["abc"]
--    ventanas 1 5 "abcdef"  == ["a","f"]
-- ---------------------------------------------------------------------

module Ventana_deslizante where

import Data.List (unfoldr)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1º solución
-- ===========

ventanas1 :: Int -> Int -> [a] -> [[a]]
ventanas1 _ _ [] = []
ventanas1 x y zs
  | length zs <= x = [zs]
  | otherwise      = take x zs : ventanas1 x y (drop y zs)

-- 2ª solución
-- ===========

ventanas2 :: Int -> Int -> [a] -> [[a]]
ventanas2 x y zs = aux (length zs) zs
  where
    aux _ [] = []
    aux n zs'
      | n <= x    = [zs']
      | otherwise = take x zs' : aux (n - y) (drop y zs')

-- 3ª solución
-- ===========

ventanas3 :: Int -> Int -> [a] -> [[a]]
ventanas3 x y = unfoldr aux
  where aux [] = Nothing
        aux xs = Just (ys,zs)
          where (ys,us)        = splitAt x xs
                zs | null us   = []
                   | otherwise = drop y xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Int -> [Int] -> [[Int]]) -> Spec
specG ventanas = do
  it "e1" $
    ventanas 4 1 [1..7]    `shouldBe` [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]
  it "e2" $
    ventanas 4 2 [1..7]    `shouldBe` [[1,2,3,4],[3,4,5,6],[5,6,7]]
  it "e3" $
    ventanas 4 3 [1..7]    `shouldBe` [[1,2,3,4],[4,5,6,7]]
  it "e4" $
    ventanas 4 4 [1..7]    `shouldBe` [[1,2,3,4],[5,6,7]]
  it "e5" $
    ventanas 4 5 [1..7]    `shouldBe` [[1,2,3,4],[6,7]]
  it "e6" $
    ventanas 4 6 [1..7]    `shouldBe` [[1,2,3,4],[7]]
  it "e7" $
    ventanas 4 7 [1..7]    `shouldBe` [[1,2,3,4]]
  it "e8" $
    ventanas 4 8 [1..7]    `shouldBe` [[1,2,3,4]]
  it "e9" $
    ventanas 3 2 [5,1,9,2] `shouldBe` [[5,1,9],[9,2]]
  it "e10" $
    ventanas 3 3 [5,1,9,2] `shouldBe` [[5,1,9],[2]]
  it "e11" $
    ventanas 3 4 [5,1,9,2] `shouldBe` [[5,1,9]]

spec :: Spec
spec = do
  describe "def. 1" $ specG ventanas1
  describe "def. 2" $ specG ventanas2
  describe "def. 3" $ specG ventanas3

-- La verificación es
--    λ> verifica
--    21 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_ventanas :: Positive Int -> Positive Int -> [Int] -> Bool
prop_ventanas (Positive x) (Positive y) zs =
  all (== ventanas1 x y zs)
      [ventanas2 x y zs,
       ventanas3 x y zs]

-- La comprobación es
--    λ> quickCheck prop_ventanas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (ventanas1 4 6 [1..10^5])
--    16667
--    (3.81 secs, 14,199,776 bytes)
--    λ> length (ventanas2 4 6 [1..10^5])
--    16667
--    (0.05 secs, 14,466,488 bytes)
--    λ> length (ventanas3 4 6 [1..10^5])
--    16667
--    (0.03 secs, 22,333,600 bytes)
--
--    λ> length (ventanas2 4 6 [1..10^7])
--    1666667
--    (1.87 secs, 1,387,268,112 bytes)
--    λ> length (ventanas3 4 6 [1..10^7])
--    1666667
--    (1.18 secs, 2,173,935,176 bytes)
