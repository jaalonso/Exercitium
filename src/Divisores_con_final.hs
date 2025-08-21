-- Divisores_con_final.hs
-- Divisores de un número con final dado.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 16 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisoresConFinal :: Integer -> Integer -> [Integer]
-- tal que (divisoresConFinal n m) es la lista de los divisores de n
-- cuyos dígitos finales coincide con m. Por ejemplo,
--    divisoresConFinal 84 4    ==  [4,14,84]
--    divisoresConFinal 720 20  ==  [20,120,720]
-- ---------------------------------------------------------------------

module Divisores_con_final where

import Data.List (group, inits, isSuffixOf, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisoresConFinal1 :: Integer -> Integer -> [Integer]
divisoresConFinal1 n m =
  [x | x <- divisores1 n, final1 x m]

-- (divisores n) es el conjunto de divisores de n. Por ejemplo,
--   divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores1 :: Integer -> [Integer]
divisores1 n = [x | x <- [1..n], n `rem` x == 0]

-- (final x y) se verifica si el final de x es igual a y. Por ejemplo,
--    final 325 5   ==  True
--    final 325 25  ==  True
--    final 325 35  ==  False
final1 :: Integer -> Integer -> Bool
final1 x y = take n xs == ys
  where xs = reverse (show x)
        ys = reverse (show y)
        n  = length ys

-- 2ª solución
-- ===========

divisoresConFinal2 :: Integer -> Integer -> [Integer]
divisoresConFinal2 n m =
  [x | x <- divisores2 n, final2 x m]

divisores2 :: Integer -> [Integer]
divisores2 n = filter ((== 0) . mod n) [1..n]

final2 :: Integer -> Integer -> Bool
final2 x y = show y `isSuffixOf` show x

-- 3ª solución
-- ===========

divisoresConFinal3 :: Integer -> Integer -> [Integer]
divisoresConFinal3 n m =
  [x | x <- divisores3 n, final2 x m]

divisores3 :: Integer -> [Integer]
divisores3 =
  nub . sort . map product . subsequences . primeFactors

-- 4ª solución
-- ===========

divisoresConFinal4 :: Integer -> Integer -> [Integer]
divisoresConFinal4 n m =
  [x | x <- divisores4 n, final2 x m]

divisores4 :: Integer -> [Integer]
divisores4 = sort
             . map (product . concat)
             . productoCartesiano
             . map inits
             . group
             . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]

-- 5ª solución
-- ===========

divisoresConFinal5 :: Integer -> Integer -> [Integer]
divisoresConFinal5 n m =
  [x | x <- divisores5 n, final2 x m]

divisores5 :: Integer -> [Integer]
divisores5 = sort
           . map (product . concat)
           . sequence
           . map inits
           . group
           . primeFactors

-- 6ª solución
-- ===========

divisoresConFinal6 :: Integer -> Integer -> [Integer]
divisoresConFinal6 n m =
  [x | x <- divisores6 n, final2 x m]

divisores6 :: Integer -> [Integer]
divisores6 = sort
           . map (product . concat)
           . mapM inits
           . group
           . primeFactors

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer -> [Integer]) -> Spec
specG divisoresConFinal = do
  it "e1" $
    divisoresConFinal 84 4    `shouldBe`  [4,14,84]
  it "e2" $
    divisoresConFinal 720 20  `shouldBe`  [20,120,720]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisoresConFinal1
  describe "def. 2" $ specG divisoresConFinal2
  describe "def. 3" $ specG divisoresConFinal3
  describe "def. 4" $ specG divisoresConFinal4
  describe "def. 5" $ specG divisoresConFinal5
  describe "def. 6" $ specG divisoresConFinal6

-- La verificación es
--    λ> verifica
--    12examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisoresConFinal :: Positive Integer -> Positive Integer -> Bool
prop_divisoresConFinal (Positive n) (Positive m) =
  all (== divisoresConFinal1 n m)
      [ divisoresConFinal2 n m,
        divisoresConFinal3 n m,
        divisoresConFinal4 n m,
        divisoresConFinal5 n m,
        divisoresConFinal6 n m ]

-- La comprobación es
--    λ> quickCheck prop_divisoresConFinal
--    +++ OK, passed 100 tests.

-- Comparación de la eficiencia
-- ============================

-- La comparación es
--    λ> divisoresConFinal1 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (13.89 secs, 7,984,560,800 bytes)
--    λ> divisoresConFinal2 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (4.84 secs, 4,790,920,688 bytes)
--    λ> divisoresConFinal3 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.07 secs, 87,137,992 bytes)
--    λ> divisoresConFinal4 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.02 secs, 2,324,528 bytes)
--    λ> divisoresConFinal5 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.00 secs, 1,801,872 bytes)
--    λ> divisoresConFinal6 (product [1..11]) 6800
--    [16800,226800,316800,39916800]
--    (0.01 secs, 1,801,536 bytes)
--
--    λ> divisoresConFinal4 (product [1..25]) 985984000000
--    [2985984000000,95096985984000000,15511210043330985984000000]
--    (1.77 secs, 2,142,500,832 bytes)
--    λ> divisoresConFinal5 (product [1..25]) 985984000000
--    [2985984000000,95096985984000000,15511210043330985984000000]
--    (1.15 secs, 1,603,330,352 bytes)
--    λ> divisoresConFinal6 (product [1..25]) 985984000000
--    [2985984000000,95096985984000000,15511210043330985984000000]
--    (1.19 secs, 1,603,329,840 bytes)

-- Referencia                                                       --
-- ==========

-- Basado en el problema 474 del proyecto Euler que se encuentra
-- en https://projecteuler.net/problem=474
