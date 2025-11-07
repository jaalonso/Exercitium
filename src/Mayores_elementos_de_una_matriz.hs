-- Mayores_elementos_de_una_matriz.hs
-- Mayores elementos de una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-Noviembre-2014 (actualizado 7-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices se pueden representar mediante listas de listas. Por
-- ejemplo, la matriz
--    |3 2 5|
--    [4 9 7|
-- se puede representar por [[3,2,5],[4,9,7]].
--
-- Definir la función
--    mayores :: Ord a => Int -> [[a]] -> [(a,Int)]
-- tal que (mayores n xss) es la lista de los n mayores elementos de la
-- matriz xss junto con sus correspondientes número de fila. Por
-- ejemplo,
--    λ> mayores 4 [[4,26,9],[2,37,53],[41,1,8]]
--    [(53,2),(41,3),(37,2),(26,1)]
--
-- Comprobar con QuickCheck que todos los elementos de (mayores n xss)
-- son mayores o iguales que los restantes elementos de xss.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mayores_elementos_de_una_matriz where

import Data.List (sort, (\\), sortBy)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

mayores1 :: Ord a => Int -> [[a]] -> [(a,Int)]
mayores1 n xss =
  take n (reverse (sort (enumeracion xss)))

-- (enumeracion xss) es la lista de los elementos de xs junto con el
-- número de su fila. Por ejemplo,
--    λ> enumeracion [[4,26,9],[2,37,53],[41,1,8]]
--    [(4,1),(26,1),(9,1),(2,2),(37,2),(53,2),(41,3),(1,3),(8,3)]
enumeracion :: [[a]] -> [(a,Int)]
enumeracion xss =
  [(x,i) | (xs,i) <- enumeracionFilas xss, x <- xs]

-- (enumeracionFilas xss) es la lista de las filas de xs junto con su
-- número. Por ejemplo,
--    λ> enumeracionFilas [[4,26,9],[2,37,53],[41,1,8]]
--    [([4,26,9],1),([2,37,53],2),([41,1,8],3)]
enumeracionFilas :: [[a]] -> [([a],Int)]
enumeracionFilas xss =
  zip xss [1..]

-- 2ª solución
-- ===========

mayores2 :: Ord a => Int -> [[a]] -> [(a,Int)]
mayores2 n xss =
  take n (reverse (sort [(x,i) | (xs,i) <- zip xss [1..], x <- xs]))

-- 3ª solución
-- ===========

mayores3 :: Ord a => Int -> [[a]] -> [(a,Int)]
mayores3 n xss =
  take n (sortBy (flip compare) (enumeracion xss))

-- 4ª solución
-- ===========

mayores4 :: Ord a => Int -> [[a]] -> [(a,Int)]
mayores4 n xss =
  take n (sortBy (flip compare) [(x, i) | (xs, i) <- zip xss [1 ..], x <- xs])

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [[Int]] -> [(Int,Int)]) -> Spec
specG mayores = do
  it "e1" $
    mayores 4 [[4,26,9],[2,37,53],[41,1,8]]
    `shouldBe` [(53,2),(41,3),(37,2),(26,1)]

spec :: Spec
spec = do
  describe "def. 1" $ specG mayores1
  describe "def. 2" $ specG mayores2
  describe "def. 3" $ specG mayores3
  describe "def. 4" $ specG mayores4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> [[Int]] -> Bool
prop_equivalencia (NonNegative n) xss =
  all (== mayores1 n xss)
      [mayores2 n xss,
       mayores3 n xss,
       mayores4 n xss]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- Genera una matriz de m filas y n con valores entre a y b. Por ejemplo,
--    λ> generate (genMatriz 2 3 0 99)
--    [[10,38,50],[70,41,61]]
--    λ> generate (genMatriz 3 2 0 99)
--    [[72,64],[26,64],[34,62]]
genMatriz :: Int -> Int -> Int -> Int -> Gen [[Int]]
genMatriz m n a b = vectorOf m (vectorOf n (choose (a, b)))

-- La comparación es
--    λ> ejemplo <- generate (genMatriz 1000 1000 0 999)
--    λ> last (mayores1 20 ejemplo)
--    (999,975)
--    (4.11 secs, 1,804,234,896 bytes)
--    λ> last (mayores2 20 ejemplo)
--    (999,975)
--    (3.75 secs, 1,522,501,216 bytes)
--    λ> last (mayores3 20 ejemplo)
--    (999,975)
--    (0.34 secs, 205,657,832 bytes)
--    λ> last (mayores4 20 ejemplo)
--    (999,975)
--    (0.31 secs, 205,657,832 bytes)

-- Propiedad
-- =========

-- La propiedad de mayores es
prop_mayores :: NonNegative Int -> [[Int]] -> Bool
prop_mayores (NonNegative n) xss =
  and [x <= y | x <- elementos \\ elementosMayores, y <- elementosMayores]
  where elementos = concat xss
        elementosMayores = [x | (x,_) <- mayores4 n xss]

-- La comprobación es
--    λ> quickCheck prop_mayores
--    +++ OK, passed 100 tests.

-- Otra forma de expresa la propiedad es
prop_mayores2 :: NonNegative Int -> [[Int]] -> Bool
prop_mayores2 (NonNegative n) xss =
  all (\x -> all (<=x) elementosRestantes) elementosMayores
  where elementosMayores   = map fst (mayores4 n xss)
        elementosRestantes = concat xss \\ elementosMayores

-- La comprobación es
--    λ> quickCheck prop_mayores2
--    +++ ok, passed 100 tests.
