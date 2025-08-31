-- Descomposiciones_con_n_sumandos.hs
-- Descomposiciones de x como sumas de n sumandos en una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-Junio-2014 (actualizado 31-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumas :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
-- tal que (sumas n ns x) es la lista de las descomposiciones de x como
-- sumas de n sumandos de la lista ns. Por ejemplo,
--    sumas 2 [1,2] 3     == [[1,2]]
--    sumas 2 [-1] (-2)   == [[-1,-1]]
--    sumas 2 [-1,3,-1] 2 == [[-1,3]]
--    sumas 2 [1,2] 4     == [[2,2]]
--    sumas 2 [1,2] 5     == []
--    sumas 3 [1,2] 5     == [[1,2,2]]
--    sumas 3 [1,2] 6     == [[2,2,2]]
--    sumas 2 [1,2,5] 6   == [[1,5]]
--    sumas 2 [1,2,3,5] 4 == [[1,3],[2,2]]
--    sumas 2 [1..5] 6    == [[1,5],[2,4],[3,3]]
--    sumas 3 [1..5] 7    == [[1,1,5],[1,2,4],[1,3,3],[2,2,3]]
--    sumas 3 [1..200] 4  == [[1,1,2]]
-- ---------------------------------------------------------------------

module Descomposiciones_con_n_sumandos where

import Data.List (nub, sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- =============

sumas1 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas1 n ns x =
  [xs | xs <- combinacionesR n (nub (sort ns))
      , sum xs == x]

-- (combinacionesR k xs) es la lista de las combinaciones orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    combinacionesR 2 "abc" == ["aa","ab","ac","bb","bc","cc"]
--    combinacionesR 3 "bc"  == ["bbb","bbc","bcc","ccc"]
--    combinacionesR 3 "abc" == ["aaa","aab","aac","abb","abc","acc",
--                               "bbb","bbc","bcc","ccc"]
combinacionesR :: Int -> [a] -> [[a]]
combinacionesR _ [] = []
combinacionesR 0 _  = [[]]
combinacionesR k (x:xs) =
  [x:ys | ys <- combinacionesR (k-1) (x:xs)] ++ combinacionesR k xs

-- 2ª solución
-- =============

sumas2 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas2 n ns x = nub (sumasAux n ns x)
  where sumasAux :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
        sumasAux 1 ns' x'
          | x' `elem` ns' = [[x']]
          | otherwise   = []
        sumasAux n' ns' x' =
          concat [[y:zs | zs <- sumasAux (n'-1) ns' (x'-y)
                        , y <= head zs]
                 | y <- ns']

-- 3ª solución
-- =============

sumas3 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas3 n ns x = nub $ aux n (sort ns) x
  where aux 0 _ _  = []
        aux _ [] _ = []
        aux 1 ys x' | x' `elem` ys = [[x']]
                    | otherwise   = []
        aux n' (y:ys) x' = aux n' ys x' ++
                           map (y:) (aux (n' - 1) (y : ys) (x' - y))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> Int -> [[Int]]) -> Spec
specG sumas = do
  it "e1" $
    sumas' 2 [1,2] 3     `shouldBe` [[1,2]]
  it "e2" $
    sumas' 2 [-1] (-2)   `shouldBe` [[-1,-1]]
  it "e3" $
    sumas' 2 [-1,3,-1] 2 `shouldBe` [[-1,3]]
  it "e4" $
    sumas' 2 [1,2] 4     `shouldBe` [[2,2]]
  it "e5" $
    sumas' 2 [1,2] 5     `shouldBe` []
  it "e6" $
    sumas' 3 [1,2] 5     `shouldBe` [[1,2,2]]
  it "e7" $
    sumas' 3 [1,2] 6     `shouldBe` [[2,2,2]]
  it "e8" $
    sumas' 2 [1,2,5] 6   `shouldBe` [[1,5]]
  it "e9" $
    sumas' 2 [1,2,3,5] 4 `shouldBe` [[1,3],[2,2]]
  it "e10" $
    sumas' 2 [1..5] 6    `shouldBe` [[1,5],[2,4],[3,3]]
  it "e11" $
    sumas' 3 [1..5] 7    `shouldBe` [[1,1,5],[1,2,4],[1,3,3],[2,2,3]]
  where sumas' n ys x = sort (map sort (sumas n ys x))

spec :: Spec
spec = do
  describe "def. 1" $ specG sumas1
  describe "def. 2" $ specG sumas2
  describe "def. 3" $ specG sumas3

-- La verificación es
--    λ> verifica
--    33 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

prop_equiv_sumas :: Positive Int -> [Int] -> Int -> Bool
prop_equiv_sumas (Positive n) ns x =
  all (== normal (sumas1 n ns x))
      [ normal (sumas2 n ns x)
      , normal (sumas3 n ns x)]
  where normal = sort . map sort

-- La verificación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_equiv_sumas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    > sumas1 3 [1..200] 4
--    [[1,1,2]]
--    (2.52 secs, 1,914,773,472 bytes)
--    > sumas2 3 [1..200] 4
--    [[1,1,2]]
--    (0.17 secs, 25,189,688 bytes)
--    λ> sumas3 3 [1..200] 4
--    [[1,1,2]]
--    (0.08 secs, 21,091,368 bytes)
