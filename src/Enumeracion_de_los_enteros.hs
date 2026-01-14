-- Enumeracion_de_los_enteros.hs
-- Enumeración de los números enteros.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-Enero-2015 (actualizado 12-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la sucesión
--    enteros :: [Int]
-- tal que sus elementos son los números enteros comenzando en el 0 e
-- Intercalando los positivos y los negativos. Por ejemplo,
--    λ> take 23 enteros
--    [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10,-10,11,-11]
--    λ> enteros !! (3*10^7)
--    -15000000
--
-- Comprobar con QuickCheck que el n-ésimo término de la sucesión es
-- (1-(2*n+1)*(-1)^n)/4.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Enumeracion_de_los_enteros where

import Data.List (transpose)
import Control.Applicative ((<**>))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

enteros1 :: [Int]
enteros1 = 0 : concat [[n,-n] | n <- [1..]]

-- 2ª solución
-- ===========

enteros2 :: [Int]
enteros2 = 0 : [y | x <- [1..], y <- [x, -x]]

-- 3ª solución
-- ===========

enteros3 :: [Int]
enteros3 = 0 : concatMap (\n -> [n, -n]) [1..]

-- 4ª solución
-- ===========

enteros4 :: [Int]
enteros4 = 0 : mezcla [1..] [-1, -2..]
  where
    mezcla (x:xs) ys = x : mezcla ys xs

-- 5ª solución
-- ============

enteros5 :: [Int]
enteros5 = 0 : concat (transpose [[1..], [-1, -2..]])

-- 6ª solución
-- ===========

enteros6 :: [Int]
enteros6 = iterate siguiente 0
  where siguiente i | i > 0     = -i
                    | otherwise = 1 - i

-- 7ª solución
-- ===========

enteros7 :: [Int]
enteros7 = iterate (\i -> if i > 0 then -i else 1-i) 0

-- 8ª solución
-- ===========

enteros8 :: [Int]
enteros8 = 0 : ([1..] <**> [id, negate])

-- 9ª solución
-- ============

enteros9 :: [Int]
enteros9 = scanl (+) 0 (zipWith (*) [1..] (cycle [1, -1]))

-- 10ª solución
-- ============

enteros10 :: [Int]
enteros10 = 0 : [f x | x <- [1..], f <- [id, negate]]

-- 11ª solución
-- ============

enteros11 :: [Int]
enteros11 = map transforma [0..]
  where
    transforma n
      | n == 0    = 0
      | odd n     = (n + 1) `div` 2
      | otherwise = -(n `div` 2)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Int] -> Spec
specG enteros = do
  it "e1" $
    take 23 enteros `shouldBe`
    [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10,-10,11,-11]

spec :: Spec
spec = do
  describe "def. 1" $ specG enteros1
  describe "def. 2" $ specG enteros2
  describe "def. 3" $ specG enteros3
  describe "def. 4" $ specG enteros4
  describe "def. 5" $ specG enteros5
  describe "def. 6" $ specG enteros6
  describe "def. 7" $ specG enteros7
  describe "def. 8" $ specG enteros8
  describe "def. 9" $ specG enteros9
  describe "def. 10" $ specG enteros10
  describe "def. 11" $ specG enteros11

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- -- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
  all (== enteros1 !! n)
      [enteros2 !! n,
       enteros3 !! n,
       enteros4 !! n,
       enteros5 !! n,
       enteros6 !! n,
       enteros7 !! n,
       enteros8 !! n,
       enteros9 !! n,
       enteros10 !! n,
       enteros11 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> enteros1 !! (5*10^6)
--    -2500000
--    (1.26 secs, 980,597,792 bytes)
--    λ> enteros2 !! (5*10^6)
--    -2500000
--    (1.79 secs, 1,020,597,592 bytes)
--    λ> enteros3 !! (5*10^6)
--    -2500000
--    (0.57 secs, 800,598,560 bytes)
--    λ> enteros4 !! (5*10^6)
--    -2500000
--    (1.97 secs, 1,020,597,576 bytes)
--    λ> enteros5 !! (5*10^6)
--    -2500000
--    (0.75 secs, 1,480,595,552 bytes)
--    λ> enteros6 !! (5*10^6)
--    -2500000
--    (5.13 secs, 2,042,951,592 bytes)
--    λ> enteros7 !! (5*10^6)
--    -2500000
--    (4.10 secs, 1,318,970,024 bytes)
--    λ> enteros8 !! (5*10^6)
--    -2500000
--    (0.46 secs, 820,598,472 bytes)
--    λ> enteros9 !! (5*10^6)
--    -2500000
--    (2.25 secs, 1,726,728,960 bytes)
--    λ> enteros10 !! (5*10^6)
--    -2500000
--    (3.09 secs, 1,240,596,624 bytes)
--    λ> enteros11 !! (5*10^6)
--    -2500000
--    (0.12 secs, 800,599,232 bytes)
--
--    λ> enteros3 !! (10^7)
--    -5000000
--    (1.85 secs, 1,600,602,280 bytes)
--    λ> enteros5 !! (10^7)
--    -5000000
--    (0.84 secs, 2,960,602,480 bytes)
--    λ> enteros8 !! (10^7)
--    -5000000
--    (0.65 secs, 1,640,602,368 bytes)
--    λ> enteros11 !! (10^7)
--    -5000000
--    (0.49 secs, 1,600,605,592 bytes)
--
--    λ> enteros5 !! (3*10^7)
--    -15000000
--    (3.36 secs, 8,880,603,304 bytes)
--    λ> enteros8 !! (3*10^7)
--    -15000000
--    (3.28 secs, 4,920,603,200 bytes)
--    λ> enteros11 !! (3*10^7)
--    -15000000
--    (2.84 secs, 4,800,606,416 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_enteros :: Positive Int -> Bool
prop_enteros (Positive n) =
  enteros1 !! n == (1-(2*n+1)*(-1)^n) `div` 4

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_enteros
--    +++ OK, passed 100 tests.
