-- N_gramas.hs
-- N gramas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-Junio-2014 (actualizado 4-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un n-grama de una sucesión es una subsucesión contigua de n elementos.
--
-- Definir la función
--    nGramas :: Int -> [a] -> [[a]]
-- tal que (nGramas k xs) es la lista de los n-gramas de xs de longitud
-- k. Por ejemplo,
--    nGramas 0 "abcd"  ==  []
--    nGramas 1 "abcd"  ==  ["a","b","c","d"]
--    nGramas 2 "abcd"  ==  ["ab", "bc", "cd"]
--    nGramas 3 "abcd"  ==  ["abc", "bcd"]
--    nGramas 4 "abcd"  ==  ["abcd"]
--    nGramas 5 "abcd"  ==  []
-- ---------------------------------------------------------------------

module N_gramas where

import Data.List (unfoldr)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

nGramas1 :: Int -> [a] -> [[a]]
nGramas1 k xs
  | k <= 0    = []
  | k > n     = []
  | otherwise = take k xs : nGramas1 k (tail xs)
  where n = length xs

-- 2ª solución
-- ===========

nGramas2 :: Int -> [a] -> [[a]]
nGramas2 k xs
  | k <= 0    = []
  | k > n     = []
  | otherwise = unfoldr aux xs
  where n = length xs
        aux ys | length ys < k = Nothing
               | otherwise     = Just (take k ys, tail ys)

-- 3ª solución
-- ===========

nGramas3 :: Int -> [a] -> [[a]]
nGramas3 k xs
  | k <= 0    = []
  | otherwise = aux k (length xs) xs
  where
    aux k' n ys
      | k' > n    = []
      | otherwise = take k' ys : aux k' (n-1) (tail ys)

-- 4ª solución
-- ===========

nGramas4 :: Int -> [a] -> [[a]]
nGramas4 k xs
  | k <= 0    = []
  | otherwise = [take k (drop i xs) | i <- [0..length xs - k]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> String -> [String]) -> Spec
specG nGramas = do
  it "e1" $
    nGramas 0 "abcd"  `shouldBe`  []
  it "e2" $
    nGramas 1 "abcd"  `shouldBe`  ["a","b","c","d"]
  it "e3" $
    nGramas 2 "abcd"  `shouldBe`  ["ab", "bc", "cd"]
  it "e4" $
    nGramas 3 "abcd"  `shouldBe`  ["abc", "bcd"]
  it "e5" $
    nGramas 4 "abcd"  `shouldBe`  ["abcd"]
  it "e6" $
    nGramas 5 "abcd"  `shouldBe`  []

spec :: Spec
spec = do
  describe "def. 1"  $ specG nGramas1
  describe "def. 2"  $ specG nGramas2
  describe "def. 3"  $ specG nGramas3
  describe "def. 4"  $ specG nGramas4

-- La verificación es
--    λ> verifica
--    24 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_nGramas :: NonNegative Int -> [Int] -> Bool
prop_nGramas (NonNegative k) xs =
  all (== nGramas1 k xs)
      [nGramas2 k xs,
       nGramas3 k xs,
       nGramas4 k xs]

-- La comprobación es
--    λ> quickCheck prop_nGramas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (nGramas1 20000 [1..40000])
--    20001
--    (3.25 secs, 10,839,512 bytes)
--    λ> length (nGramas2 20000 [1..40000])
--    20001
--    (3.25 secs, 10,519,720 bytes)
--    λ> length (nGramas3 20000 [1..40000])
--    20001
--    (0.04 secs, 11,159,656 bytes)
--    λ> length (nGramas4 20000 [1..40000])
--    20001
--    (0.03 secs, 7,479,488 bytes)
--
--    λ> length (nGramas3 1000000 [1..10000000])
--    9000001
--    (6.87 secs, 4,176,601,176 bytes)
--    λ> length (nGramas4 1000000 [1..10000000])
--    9000001
--    (3.25 secs, 2,520,601,008 bytes)
