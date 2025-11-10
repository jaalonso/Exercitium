-- Minimos_locales.hs
-- Mínimos locales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-Noviembre-2014 (actualizado 9-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un mínimo local de una lista es un elemento de la lista que es menor
-- que su predecesor y que su sucesor en la lista. Por ejemplo, 1 es un
-- mínimo local de [3,2,1,3,7,7,1,0,2] ya que es menor  que 2 (su
-- predecesor) y que 3 (su sucesor).
--
-- Definir la función
--    minimosLocales :: Ord a => [a] -> [a]
-- tal que (minimosLocales xs) es la lista de los mínimos locales de la
-- lista xs. Por ejemplo,
--    minimosLocales [3,2,1,3,7,7,9,6,8]  ==  [1,6]
--    minimosLocales [1..100]             ==  []
--    minimosLocales "mqexvzat"           ==  "eva"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Minimos_locales where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========
minimosLocales1 :: Ord a => [a] -> [a]
minimosLocales1 (x:y:z:xs)
  | y < x && y < z = y : minimosLocales1 (z:xs)
  | otherwise      = minimosLocales1 (y:z:xs)
minimosLocales1 _  = []

-- 2ª solución
-- ===========

minimosLocales2 :: Ord a => [a] -> [a]
minimosLocales2 xs =
  [y | (x,y,z) <- ternas xs, y < x, y < z]

-- (ternas xs) es la lista de las ternas de xs. Por ejemplo,
--    λ> ternas [3,2,1,3,7,7,9,6,8]
--    [(3,2,1),(2,1,3),(1,3,7),(3,7,7),(7,7,9),(7,9,6),(9,6,8)]
ternas :: [a] -> [(a,a,a)]
ternas (x:y:z:ts) = (x,y,z) : ternas (y:z:ts)
ternas _          = []

-- 3ª solución
-- ===========

minimosLocales3 :: Ord a => [a] -> [a]
minimosLocales3 xs =
  [y | (x,y,z) <- ternas2 xs, y < x, y < z]

ternas2 :: [a] -> [(a,a,a)]
ternas2 xs =
  zip3 xs (tail xs) (drop 2 xs)

-- 4ª solución
-- ===========

minimosLocales4 :: Ord a => [a] -> [a]
minimosLocales4 xs =
  [y | (x,y,z) <- zip3 xs (tail xs) (drop 2 xs), y < x, y < z]

-- 5ª solución
-- ===========

minimosLocales5 :: Ord a => [a] -> [a]
minimosLocales5 xs =
  concatMap (\(x, y, z) -> [y | y < x, y < z])
            (zip3 xs (tail xs) (drop 2 xs))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG minimosLocales = do
  it "e1" $
    minimosLocales [3,2,1,3,7,7,9,6,8]  `shouldBe`  [1,6]
  it "e2" $
    minimosLocales [1,2,3]              `shouldBe`  []

spec :: Spec
spec = do
  describe "def. 1" $ specG minimosLocales1
  describe "def. 2" $ specG minimosLocales2
  describe "def. 3" $ specG minimosLocales3
  describe "def. 4" $ specG minimosLocales4
  describe "def. 5" $ specG minimosLocales5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
  all (== minimosLocales1 xs)
      [minimosLocales2 xs,
       minimosLocales3 xs,
       minimosLocales4 xs,
       minimosLocales5 xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (minimosLocales1 (take (5*10^6) (cycle [2,1,3])))
--    1666666
--    (1.99 secs, 1,587,267,920 bytes)
--    λ> length (minimosLocales2 (take (5*10^6) (cycle [2,1,3])))
--    1666666
--    (2.68 secs, 2,507,267,648 bytes)
--    λ> length (minimosLocales3 (take (5*10^6) (cycle [2,1,3])))
--    1666666
--    (1.66 secs, 1,347,268,160 bytes)
--    λ> length (minimosLocales4 (take (5*10^6) (cycle [2,1,3])))
--    1666666
--    (1.64 secs, 1,347,268,248 bytes)
--    λ> length (minimosLocales5 (take (5*10^6) (cycle [2,1,3])))
--    1666666
--    (1.59 secs, 1,920,601,368 bytes)
