-- Emparejamiento_binario.hs
-- Emparejamiento binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-mayo-2014 (actualizado 28-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
-- tal que (zipBinario fs xs ys) es la lista obtenida aplicando cada una
-- de las operaciones binarias de fs a los correspondientes elementos de
-- xs e ys. Por ejemplo,
--    zipBinario [(+), (*), (*)] [2,2,2] [4,4,4]    == [6,8,8]
--    zipBinario [(+)] [2,2,2] [4,4,4]              == [6]
--    zipBinario [(<), (==), (==)] "coloca" "lobo"  == [True,True,False]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Emparejamiento_binario where

import Test.QuickCheck
import Test.QuickCheck.HigherOrder
import Test.Hspec

-- 1ª solución
-- ===========

zipBinario1 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario1 (f:fs) (x:xs) (y:ys) = f x y : zipBinario1 fs xs ys
zipBinario1 _ _ _                = []

-- 2ª solución
-- ===========

zipBinario2 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario2 fs xs ys = [f x y | (f,(x,y)) <- zip fs (zip xs ys)]

-- 3ª solución
-- ===========

zipBinario3 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario3 fs xs ys = [f x y | (f,x,y) <- zip3 fs xs ys]

-- 4ª solución
-- ===========

zipBinario4 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario4 = zipWith3 id

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int -> Int -> Int] -> [Int] -> [Int] -> [Int]) -> Spec
specG zipBinario = do
  it "e1" $ zipBinario [(+), (*), (*)] [2,2,2] [4,4,4]    `shouldBe` [6,8,8]
  it "e2" $ zipBinario [(+)] [2,2,2] [4,4,4]              `shouldBe` [6]

spec :: Spec
spec = do
  describe "def. 1" $ specG zipBinario1
  describe "def. 2" $ specG zipBinario2
  describe "def. 3" $ specG zipBinario3
  describe "def. 4" $ specG zipBinario4

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_zipBinario :: [Int -> Int -> Int] -> [Int] -> [Int] -> Bool
prop_zipBinario fs xs ys =
  all (== zipBinario1 fs xs ys)
      [g fs xs ys | g <- [zipBinario2,
                          zipBinario3,
                          zipBinario4]]

-- La comprobación es
--    λ> quickCheck' prop_zipBinario
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximum (zipBinario1 (cycle [(+), (*)]) [1..] [1..2*10^6])
--    4000000000000
--    (2.13 secs, 965,392,072 bytes)
--    λ> maximum (zipBinario2 (cycle [(+), (*)]) [1..] [1..2*10^6])
--    4000000000000
--    (1.86 secs, 1,109,392,176 bytes)
--    λ> maximum (zipBinario3 (cycle [(+), (*)]) [1..] [1..2*10^6])
--    4000000000000
--    (1.93 secs, 981,392,128 bytes)
--    λ> maximum (zipBinario4 (cycle [(+), (*)]) [1..] [1..2*10^6])
--    4000000000000
--    (1.07 secs, 773,392,040 bytes)
