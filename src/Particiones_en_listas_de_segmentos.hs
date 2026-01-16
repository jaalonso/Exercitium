-- Particiones_en_listas_de_segmentos.hs
-- Particiones en listas de segmentos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-Enero-2015 (actualizado 16-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    particiones :: Int -> [a] -> [[[a]]]
-- tal que (particiones n xs) es la lista de lista de n elementos cuya
-- concatenación es xs. Por ejemplo,
--
--    λ> particiones 2 "abc"
--    [["","abc"],["a","bc"],["ab","c"],["abc",""]]
--    λ> particiones 3 "abc"
--    [["","","abc"],["","a","bc"],["","ab","c"],["","abc",""],
--     ["a","","bc"],["a","b","c"],["a","bc",""],["ab","","c"],
--     ["ab","c",""],["abc","",""]]
--    λ> length (particiones 4 "abc")
--    20
-- ---------------------------------------------------------------------

module Particiones_en_listas_de_segmentos where

import Data.List (inits, tails)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

particiones1 :: Int -> [a] -> [[[a]]]
particiones1 1 xs = [[xs]]
particiones1 n xs = [ys:yss | (ys,zs) <- biparticiones1 xs,
                              yss <- particiones1 (n-1) zs]

-- (biparticiones xs) es la lista de los pares (xs1,xs2) tales que su
-- concatenación es xs. Por ejemplo,
--    λ> biparticiones "abc"
--    [("","abc"),("a","bc"),("ab","c"),("abc","")]
biparticiones1 :: [a] -> [([a],[a])]
biparticiones1 []     = [([],[])]
biparticiones1 (x:xs) = ([],x:xs) :
                        [(x:xs1,xs2) | (xs1,xs2) <- biparticiones1 xs]

-- 2ª solución
-- ===========

particiones2 :: Int -> [a] -> [[[a]]]
particiones2 1 xs = [[xs]]
particiones2 n xs = [ys:yss | (ys,zs) <- biparticiones2 xs,
                              yss <- particiones2 (n-1) zs]

biparticiones2 :: [a] -> [([a],[a])]
biparticiones2 xs =
  [splitAt i xs | i <- [0..length xs]]

-- 3ª solución
-- ===========

particiones3 :: Int -> [a] -> [[[a]]]
particiones3 1 xs = [[xs]]
particiones3 n xs = [ys:yss | (ys,zs) <- biparticiones3 xs,
                              yss <- particiones3 (n-1) zs]

biparticiones3 :: [a] -> [([a],[a])]
biparticiones3 xs =
  [splitAt i xs | i <- [0..length xs]]

-- 4ª solución
-- ===========

particiones4 :: Int -> [a] -> [[[a]]]
particiones4 1 xs = [[xs]]
particiones4 n xs = [ys:yss | (ys,zs) <- biparticiones4 xs,
                              yss <- particiones4 (n-1) zs]

biparticiones4 :: [a] -> [([a],[a])]
biparticiones4 xs =
  zip (inits xs) (tails xs)

-- 5ª solución
-- ===========

particiones5 :: Int -> [a] -> [[[a]]]
particiones5 1 xs = [[xs]]
particiones5 n xs = [ys:yss | (ys,zs) <- biparticiones5 xs,
                              yss <- particiones5 (n-1) zs]

biparticiones5 :: [a] -> [([a],[a])]
biparticiones5 = liftA2 zip inits tails

-- 6ª solución
-- ===========

particiones6 :: Int -> [a] -> [[[a]]]
particiones6 1 xs = [[xs]]
particiones6 n xs = [ys:yss | (ys,zs) <- biparticiones6 xs,
                              yss <- particiones6 (n-1) zs]

biparticiones6 :: [a] -> [([a],[a])]
biparticiones6 = zip <$> inits <*> tails

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> String -> [[String]]) -> Spec
specG particiones = do
  it "e1" $
    particiones 2 "abc" `shouldBe`
    [["","abc"],["a","bc"],["ab","c"],["abc",""]]
  it "e2" $
    particiones 3 "abc" `shouldBe`
    [["","","abc"],["","a","bc"],["","ab","c"],["","abc",""],
     ["a","","bc"],["a","b","c"],["a","bc",""],["ab","","c"],
     ["ab","c",""],["abc","",""]]
  it "ee" $
    length (particiones 4 "abc") `shouldBe` 20

spec :: Spec
spec = do
  describe "def. 1" $ specG particiones1
  describe "def. 2" $ specG particiones2
  describe "def. 3" $ specG particiones3
  describe "def. 4" $ specG particiones4
  describe "def. 5" $ specG particiones5
  describe "def. 6" $ specG particiones6

-- La verificación es
--    λ> verifica
--    18 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> [Int] -> Bool
prop_equivalencia (Positive n) xs =
  all (== particiones1 n xs)
      [particiones2 n xs,
       particiones3 n xs,
       particiones4 n xs,
       particiones5 n xs,
       particiones6 n xs]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (particiones1 5 [1..70])
--    1150626
--    (2.82 secs, 3,779,641,232 bytes)
--    λ> length (particiones2 5 [1..70])
--    1150626
--    (1.07 secs, 1,266,488,112 bytes)
--    λ> length (particiones3 5 [1..70])
--    1150626
--    (1.15 secs, 1,457,374,840 bytes)
--    λ> length (particiones4 5 [1..70])
--    1150626
--    (0.96 secs, 1,245,874,088 bytes)
--    λ> length (particiones5 5 [1..70])
--    1150626
--    (0.93 secs, 1,244,836,904 bytes)
--    λ> length (particiones6 5 [1..70])
--    1150626
--    (0.94 secs, 1,246,911,272 bytes)
--
--    λ> length (particiones4 5 [1..80])
--    1929501
--    (1.57 secs, 2,080,655,488 bytes)
--    λ> length (particiones5 5 [1..80])
--    1929501
--    (1.52 secs, 2,079,130,944 bytes)
--    λ> length (particiones6 5 [1..80])
--    1929501
--    (1.54 secs, 2,082,180,032 bytes)
--
--    λ> length (particiones4 7 [1..40])
--    9366819
--    (9.66 secs, 12,898,484,192 bytes)
--    λ> length (particiones5 7 [1..40])
--    9366819
--    (9.66 secs, 12,876,552,128 bytes)
--    λ> length (particiones6 7 [1..40])
--    9366819
--    (9.71 secs, 12,920,416,256 bytes)
