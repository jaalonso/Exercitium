-- Permutacion_de_consecutivos.hs
-- Permutación de elementos consecutivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-Diciembre-2014 (actualizado 9-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    permutaConsecutivos :: [a] -> [a]
-- tal que (permutaConsecutivos xs) es la lista obtenida permutando los
-- elementos consecutivos de xs. Por ejemplo,
--    permutaConsecutivos [1..8]         ==  [2,1,4,3,6,5,8,7]
--    permutaConsecutivos [1..9]         ==  [2,1,4,3,6,5,8,7,9]
--    permutaConsecutivos "simplemente"  ==  "ispmelemtne"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Permutacion_de_consecutivos where

import Data.List (unfoldr, transpose)
import Data.List.Split (chunksOf)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

permutaConsecutivos1 :: [a] -> [a]
permutaConsecutivos1 (x:y:zs) = y : x : permutaConsecutivos1 zs
permutaConsecutivos1 xs       = xs

-- 2ª solución
-- ===========

permutaConsecutivos2 :: [a] -> [a]
permutaConsecutivos2 xs =
  concat (transpose [impares xs, pares xs])

-- (pares xs) es la lista de los elementos de xs en posiciones
-- pares. Por ejemplo,
--    pares [2,3,5,0]   == [2,5]
--    pares [2,3,5,0,7] == [2,5,7]
pares :: [a] -> [a]
pares (x:_:zs) = x : pares zs
pares xs       = xs

-- (impares xs) es la lista de los elementos de xs en posiciones
-- impares. Por ejemplo,
--    impares [2,3,5,0]   == [3,0]
--    impares [2,3,5,0,7] == [3,0]
impares :: [a] -> [a]
impares = pares . drop 1

-- 3ª solución
-- ===========

permutaConsecutivos3 :: [a] -> [a]
permutaConsecutivos3 = concat . unfoldr paso
  where
    paso (x:y:zs) = Just ([y,x], zs)
    paso [x]      = Just ([x], [])
    paso []       = Nothing

-- 4ª solución
-- ===========

permutaConsecutivos4 :: [a] -> [a]
permutaConsecutivos4 = concatMap reverse . chunksOf 2

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG permutaConsecutivos = do
  it "e1" $
    permutaConsecutivos [1..8] `shouldBe` [2,1,4,3,6,5,8,7]
  it "e2" $
    permutaConsecutivos [1..9] `shouldBe` [2,1,4,3,6,5,8,7,9]

spec :: Spec
spec = do
  describe "def. 1" $ specG permutaConsecutivos1
  describe "def. 2" $ specG permutaConsecutivos2
  describe "def. 3" $ specG permutaConsecutivos3
  describe "def. 4" $ specG permutaConsecutivos4

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
  all (== permutaConsecutivos1 xs)
      [permutaConsecutivos2 xs,
       permutaConsecutivos3 xs,
       permutaConsecutivos4 xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (permutaConsecutivos1 [1..10^7])
--    10000000
--    (1.86 secs, 1,680,602,184 bytes)
--    λ> length (permutaConsecutivos2 [1..10^7])
--    10000000
--    (2.90 secs, 4,040,602,384 bytes)
--    λ> length (permutaConsecutivos3 [1..10^7])
--    10000000
--    (1.48 secs, 2,520,602,232 bytes)
--    λ> length (permutaConsecutivos4 [1..10^7])
--    10000000
--    (0.50 secs, 2,960,602,288 bytes)
