-- Maximos_locales.hs
-- Máximos locales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-febrero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un máximo local de una lista es un elemento de la lista que es mayor
-- que su predecesor y que su sucesor en la lista. Por ejemplo, 5 es un
-- máximo local de [3,2,5,3,7,7,1,6,2] ya que es mayor que 2 (su
-- predecesor) y que 3 (su sucesor).
--
-- Definir la función
--    maximosLocales :: Ord a => [a] -> [a]
-- tal que (maximosLocales xs) es la lista de los máximos locales de la
-- lista xs. Por ejemplo,
--    maximosLocales [3,2,5,3,7,7,1,6,2]  ==  [5,6]
--    maximosLocales [1..100]             ==  []
--    maximosLocales "adbpmqexyz"         ==  "dpq"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Maximos_locales where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

maximosLocales1 :: Ord a => [a] -> [a]
maximosLocales1 (x:y:z:xs)
  | y > x && y > z = y : maximosLocales1 (z:xs)
  | otherwise      = maximosLocales1 (y:z:xs)
maximosLocales1 _ = []

-- 2ª solución
-- ===========

maximosLocales2 :: Ord a => [a] -> [a]
maximosLocales2 xs =
  [y | (x,y,z) <- zip3 xs (tail xs) (drop 2 xs), y > x, y > z]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG maximosLocales = do
  it "e1" $
    maximosLocales [3,2,5,3,7,7,1,6,2]  `shouldBe`  [5,6]
  it "e2" $
    maximosLocales [1..100]             `shouldBe`  []

spec :: Spec
spec = do
  describe "def. 1" $ specG maximosLocales1
  describe "def. 2" $ specG maximosLocales2

-- La verificación es
--    λ> verifica
--
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_maximosLocales :: [Int] -> Property
prop_maximosLocales xs =
  maximosLocales1 xs === maximosLocales2 xs

-- La comprobación es
--    λ> quickCheck prop_maximosLocales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (maximosLocales1 (take (6*10^6) (cycle "abc")))
--    'c'
--    (3.26 secs, 1,904,464,984 bytes)
--    λ> last (maximosLocales2 (take (6*10^6) (cycle "abc")))
--    'c'
--    (2.79 secs, 1,616,465,088 bytes)
