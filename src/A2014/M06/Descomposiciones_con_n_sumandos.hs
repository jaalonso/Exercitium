-- Descomposiciones_con_n_sumandos.hs
-- Descomposiciones de x como sumas de n sumandos de una lista.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 17 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumas :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
-- tal que (sumas n ns x) es la lista de las descomposiciones de x como
-- sumas de n sumandos en la lista ordenada ns. Por ejemplo,
--    sumas 2 [1,2] 3      ==  [[1,2],[2,1]]
--    sumas 2 [1,2] 4      ==  [[2,2]]
--    sumas 2 [1,2] 5      ==  []
--    sumas 3 [1,2] 5      ==  [[1,2,2],[2,1,2],[2,2,1]]
--    sumas 3 [1,2] 6      ==  [[2,2,2]]
--    sumas 2 [1,2,5] 6    ==  [[1,5],[5,1]]
--    sumas 3 [1..1000] 4  ==  [[1,1,2],[1,2,1],[2,1,1]]
-- ---------------------------------------------------------------------

module A2014.M06.Descomposiciones_con_n_sumandos where

import qualified Data.Map as Map
import Control.Monad (replicateM)
import Data.List (sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumas1 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas1 n ns x =
  [ys | ys <- variaciones1 n ns, sum ys == x]

-- (variaciones n xs) es la lista de las variaciones con repetición
-- de n elementos de xs. Por ejemplo,
--    λ> variaciones 2 [1,2]
--    [[1,1],[1,2],[2,1],[2,2]]
--    λ> variaciones 3 [1,2]
--    [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
variaciones1 :: Int -> [a] -> [[a]]
variaciones1 0 _  = [[]]
variaciones1 k xs =
  [z:ys | z <- xs, ys <- variaciones1 (k-1) xs]

-- 2ª solución
-- ===========

sumas2 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas2 n ns x = filter ((== x) . sum) (variaciones2 n ns)

variaciones2 :: Int -> [a] -> [[a]]
variaciones2 = replicateM

-- 3ª solución
-- ===========

sumas3 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas3 0 _ 0 = [[]]
sumas3 0 _ _ = []
sumas3 _ _ 0 = []
sumas3 n ns x =
  [ y : ys | y <- ns, y <= x, ys <- sumas3 (n-1) ns (x-y) ]

-- 4ª solución
-- ===========

sumas4 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas4 1 ys x | x `elem` ys = [[x]]
              | otherwise   = []
sumas4 n ys x =
  [y:zs | y <- ys, y <= x, zs <- sumas4 (n-1) ys (x-y)]

-- 5ª solución
-- ===========

sumas5 :: (Num a, Ord a, Eq a) => Int -> [a] -> a -> [[a]]
sumas5 n ns  = aux n
  where
    aux 0 0 = [[]]
    aux 0 _ = []
    aux k x = [y : ys | y <- takeWhile (<= x) ns,
                        ys <- aux (k-1) (x-y)]

-- 6ª solución
-- ===========

sumas6 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas6 n ns x = aux n x []
  where
    aux 0 0 yss   = [reverse yss]
    aux 0 _ _     = []
    aux n' x' yss = concat [aux (n'-1) (x'-s) (s:yss) | s <- ns]

-- 7ª solución
-- ===========

sumas7 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas7 n ns x = aux n x Map.empty
  where
    aux 0 0 _ = [[]]
    aux 0 _ _ = []
    aux n' x' dic = case Map.lookup (n', x') dic of
      Just yss -> yss
      Nothing -> [y : ys | y <- ns, ys <- aux (n' - 1) (x' - y) dic]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> Int -> [[Int]]) -> Spec
specG sumas = do
  it "e1" $
    sumas 2 [1,2] 3    `shouldBe`  [[1,2],[2,1]]
  it "e2" $
    sumas 2 [1,2] 4    `shouldBe`  [[2,2]]
  it "e3" $
    sumas 2 [1,2] 5    `shouldBe`  []
  it "e4" $
    sumas 3 [1,2] 5    `shouldBe`  [[1,2,2],[2,1,2],[2,2,1]]
  it "e5" $
    sumas 3 [1,2] 6    `shouldBe`  [[2,2,2]]
  it "e6" $
    sumas 2 [1,2,5] 6  `shouldBe`  [[1,5],[5,1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumas1
  describe "def. 2" $ specG sumas2
  describe "def. 3" $ specG sumas3
  describe "def. 4" $ specG sumas4
  describe "def. 5" $ specG sumas5
  describe "def. 6" $ specG sumas6
  describe "def. 7" $ specG sumas7

-- La verificación es
--    λ> verifica
--    12examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumas :: Positive Int -> [Int] -> Positive Int -> Bool
prop_sumas (Positive n) ns (Positive x) =
  all (== sumas1 n ms x)
      [ sumas2 n ms x,
        sumas3 n ms x,
        sumas4 n ms x,
        sumas5 n ms x,
        sumas6 n ms x,
        sumas7 n ms x
      ]
  where ms = sort (map (\ y -> 1 + abs y) ns)

-- La comprobación es
--    λ> quickCheck prop_sumas
--    +++ OK, passed 100 tests.

-- Comparación de la eficiencia
-- ============================

-- La comparación es
--    λ> sumas1 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (13.89 secs, 7,984,560,800 bytes)
--    λ> sumas2 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (4.84 secs, 4,790,920,688 bytes)
--    λ> sumas3 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.07 secs, 87,137,992 bytes)
--    λ> sumas4 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.02 secs, 2,324,528 bytes)
--    λ> sumas5 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.00 secs, 1,801,872 bytes)
--    λ> sumas6 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.01 secs, 1,801,536 bytes)
--
--    λ> sumas4 (product [1..25]) 985984000000
--    [2985984000000,95096985984000000,15511210043330985984000000]
--    (1.77 secs, 2,142,500,832 bytes)
--    λ> sumas5 (product [1..25]) 985984000000
--    [2985984000000,95096985984000000,15511210043330985984000000]
--    (1.15 secs, 1,603,330,352 bytes)
--    λ> sumas6 (product [1..25]) 985984000000
--    [2985984000000,95096985984000000,15511210043330985984000000]
--    (1.19 secs, 1,603,329,840 bytes)
