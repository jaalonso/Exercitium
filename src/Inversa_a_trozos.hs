-- Inversa_a_trozos.hs
-- Inversa a trozos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-Noviembre-2014 (actualizado 10-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    inversa :: Int -> [a] -> [a]
-- tal que (inversa k xs) es la lista obtenida invirtiendo elementos de
-- xs, k elementos cada vez. Si el número de elementos de xs no es un
-- múltiplo de k, entonces los finales elementos de xs se dejen sin
-- invertir. Por ejemplo,
--    inversa 3 [1..11]  ==  [3,2,1,6,5,4,9,8,7,10,11]
--    inversa 4 [1..11]  ==  [4,3,2,1,8,7,6,5,9,10,11]
--
-- Comprobar con QuickCheck que la función inversa es involutiva; es
-- decir, para todo número k>0 y toda lista xs, se tiene que
-- (inversa k (inversa k xs)) es igual a xs
-- ---------------------------------------------------------------------

module Inversa_a_trozos where

import Data.List.Split (chunksOf)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

inversa1 :: Int -> [a] -> [a]
inversa1 k xs
  | length xs < k = xs
  | otherwise     = reverse (take k xs) ++ inversa1 k (drop k xs)

-- 2ª solución
-- ===========

inversa2 :: Int -> [a] -> [a]
inversa2 k xs = aux xs (length xs)
  where
    aux ys n
      | n < k     = ys
      | otherwise = reverse (take k ys) ++ aux (drop k ys) (n-k)

-- 3ª solución
-- ===========

inversa3 :: Int -> [a] -> [a]
inversa3 k xs =
  concatMap invierteSiCompleto (divideEnGrupos k xs)
  where
    invierteSiCompleto ys | length ys == k = reverse ys
                          | otherwise      = ys

divideEnGrupos :: Int -> [a] -> [[a]]
divideEnGrupos _ [] = []
divideEnGrupos n xs =
  ys : divideEnGrupos n zs
  where (ys, zs) = splitAt n xs

-- 4ª solución
-- ===========

inversa4 :: Int -> [a] -> [a]
inversa4 k xs =
  concatMap invierteSiCompleto (chunksOf k xs)
  where
    invierteSiCompleto ys | length ys == k = reverse ys
                          | otherwise      = ys

-- 5ª solución
-- ===========

inversa5 :: Int -> [a] -> [a]
inversa5 k xs =
  aux (chunksOf k xs)
  where
    aux [] = []
    aux [ys] | length ys == k = reverse ys
             | otherwise      = ys
    aux (ys:yss) = reverse ys ++ aux yss

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> [Int]) -> Spec
specG inversa = do
  it "e1" $
    inversa 3 [1..11]  `shouldBe`  [3,2,1,6,5,4,9,8,7,10,11]
  it "e2" $
    inversa 4 [1..11]  `shouldBe`  [4,3,2,1,8,7,6,5,9,10,11]

spec :: Spec
spec = do
  describe "def. 1" $ specG inversa1
  describe "def. 2" $ specG inversa2
  describe "def. 3" $ specG inversa3
  describe "def. 4" $ specG inversa4
  describe "def. 5" $ specG inversa5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> [Int] -> Bool
prop_equivalencia (Positive k) xs =
  all (== inversa1 k xs)
      [inversa2 k xs,
       inversa3 k xs,
       inversa4 k xs,
       inversa5 k xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (inversa1 3 [1..100000])
--    100000
--    (7.10 secs, 31,799,800 bytes)
--    λ> last (inversa2 3 [1..100000])
--    100000
--    (0.07 secs, 33,133,192 bytes)
--    λ> last (inversa3 3 [1..100000])
--    100000
--    (0.08 secs, 40,600,424 bytes)
--    λ> last (inversa4 3 [1..100000])
--    100000
--    (0.06 secs, 32,067,024 bytes)
--    λ> last (inversa5 3 [1..100000])
--    100000
--    (0.05 secs, 31,533,576 bytes)
--
--    λ> last (inversa2 3 [1..5*10^6])
--    5000000
--    (1.97 secs, 1,627,268,040 bytes)
--    λ> last (inversa3 3 [1..5*10^6])
--    5000000
--    (2.01 secs, 2,000,602,008 bytes)
--    λ> last (inversa4 3 [1..5*10^6])
--    5000000
--    (0.93 secs, 1,573,935,312 bytes)
--    λ> last (inversa5 3 [1..5*10^6])
--    5000000
--    (0.90 secs, 1,547,268,440 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_inversa :: Positive Int -> [Int] -> Bool
prop_inversa (Positive k) xs =
  inversa4 k (inversa4 k xs) == xs

-- La comprobación es
--    λ> quickCheck prop_inversa
--    +++ OK, passed 100 tests.
