-- Todas_tienen_par.hs
-- Todas tienen par.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-Julio-2014 (actualizado 24-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    todasTienenPar :: [[Int]] -> Bool
-- tal que tal que (todasTienenPar xss) se verifica si cada elemento de
-- la lista de listas xss contiene algún número par. Por ejemplo,
--    todasTienenPar [[1,2],[3,4,5],[8]]  ==  True
--    todasTienenPar [[1,2],[3,5]]        ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Todas_tienen_par where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

todasTienenPar1 :: [[Int]] -> Bool
todasTienenPar1 xss =
  and [or [even x | x <- xs] | xs <- xss]

-- 2ª solución
-- ===========

todasTienenPar2 :: [[Int]] -> Bool
todasTienenPar2 []       = True
todasTienenPar2 (xs:xss) = tienePar xs && todasTienenPar2 xss

-- (tienePar xs) se verifica si xs contiene algún número par.
tienePar  :: [Int] -> Bool
tienePar []     = False
tienePar (x:xs) = even x || tienePar xs

-- 3ª solución
-- ===========

todasTienenPar3 :: [[Int]] -> Bool
todasTienenPar3 = foldr ((&&) . tienePar3) True

-- (tienePar3 xs) se verifica si xs contiene algún número par.
tienePar3  :: [Int] -> Bool
tienePar3 = foldr ((||) . even) False

-- 4ª solución
-- ===========

todasTienenPar4 :: [[Int]] -> Bool
todasTienenPar4 = all (any even)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> Bool) -> Spec
specG todasTienenPar = do
  it "e1" $
    todasTienenPar [[1,2],[3,4,5],[8]]  `shouldBe` True
  it "e2" $
    todasTienenPar [[1,2],[3,5]]        `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG todasTienenPar1
  describe "def. 2" $ specG todasTienenPar2
  describe "def. 3" $ specG todasTienenPar3
  describe "def. 4" $ specG todasTienenPar4

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_todasTienenPar :: [[Int]] -> Bool
prop_todasTienenPar xss =
  all (== todasTienenPar1 xss)
      [ todasTienenPar2 xss
      , todasTienenPar3 xss
      , todasTienenPar4 xss]

-- La comprobación es
--    λ> quickCheck prop_todasTienenPar
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> todasTienenPar1 (replicate 4000 ([1,3..4000] ++ [0]))
--    True
--    (2.02 secs, 2,691,974,232 bytes)
--    λ> todasTienenPar2 (replicate 4000 ([1,3..4000] ++ [0]))
--    True
--    (2.63 secs, 2,114,663,016 bytes)
--    λ> todasTienenPar3 (replicate 4000 ([1,3..4000] ++ [0]))
--    True
--    (0.65 secs, 1,858,502,376 bytes)
--    λ> todasTienenPar4 (replicate 4000 ([1,3..4000] ++ [0]))
--    True
--    (0.47 secs, 1,794,470,264 bytes)
