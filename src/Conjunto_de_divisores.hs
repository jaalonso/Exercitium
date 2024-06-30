-- Conjunto_de_divisores.hs
-- Conjunto de divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-junio-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisores :: Integer -> [Integer]
-- tal que (divisores x) es el conjunto de divisores de los x. Por
-- ejemplo,
--   divisores 30  ==  [1,2,3,5,6,10,15,30]
--   length (divisores (product [1..10]))  ==  270
--   length (divisores (product [1..25]))  ==  340032
-- ---------------------------------------------------------------------

module Conjunto_de_divisores where

import Data.List (group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisores1 :: Integer -> [Integer]
divisores1 n = [x | x <- [1..n], n `rem` x == 0]

-- 2ª solución
-- ===========

divisores2 :: Integer -> [Integer]
divisores2 n = filter ((== 0) . mod n) [1..n]

-- 3ª solución
-- ===========

divisores3 :: Integer -> [Integer]
divisores3 =
  nub . sort . map product . subsequences . primeFactors

-- 4ª solución
-- ===========

divisores4 :: Integer -> [Integer]
divisores4 =
  sort
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

divisores5 :: Integer -> [Integer]
divisores5 = sort
           . map (product . concat)
           . sequence
           . map inits
           . group
           . primeFactors

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG divisores = do
  it "e1" $
    divisores 30  `shouldBe`  [1,2,3,5,6,10,15,30]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisores1
  describe "def. 2" $ specG divisores2
  describe "def. 3" $ specG divisores3
  describe "def. 4" $ specG divisores4
  describe "def. 5" $ specG divisores5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisores :: Positive Integer -> Bool
prop_divisores (Positive n) =
  all (== divisores1 n)
      [ divisores2 n
      , divisores3 n
      , divisores4 n
      , divisores5 n
      ]

-- La comprobación es
--    λ> quickCheck prop_divisores
--    +++ OK, passed 100 tests.

-- Comparación de la eficiencia
-- ============================

--    λ> length (divisores (product [1..11]))
--    540
--    (12.51 secs, 7,983,499,736 bytes)
--    λ> length (divisores2 (product [1..11]))
--    540
--    (4.81 secs, 4,790,146,656 bytes)
--    λ> length (divisores3 (product [1..11]))
--    540
--    (0.10 secs, 107,339,848 bytes)
--    λ> length (divisores4 (product [1..11]))
--    540
--    (0.02 secs, 1,702,616 bytes)
--    λ> length (divisores5 (product [1..11]))
--    540
--    (0.02 secs, 1,205,824 bytes)
--
--    λ> length (divisores3 (product [1..14]))
--    2592
--    (7.89 secs, 9,378,454,912 bytes)
--    λ> length (divisores4 (product [1..14]))
--    2592
--    (0.03 secs, 9,426,528 bytes)
--    λ> length (divisores5 (product [1..14]))
--    2592
--    (0.02 secs, 6,636,608 bytes)
--
--    λ> length (divisores4 (product [1..25]))
--    340032
--    (1.65 secs, 2,055,558,208 bytes)
--    λ> length (divisores5 (product [1..25]))
--    340032
--    (0.88 secs, 1,532,515,304 bytes)
