-- Sustitucion_de_listas.hs
-- Sustitución de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-Enero-2015 (actualizado 11-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sustituye :: Eq a => [a] -> [a] -> [a] -> [a]
-- tal que (sustituye xs ys zs) es la lista obtenida sustituyendo las
-- ocurrencias de la lista no vacía xs en zs por ys. Por ejemplo,
--    sustituye "as" "_" "las casaderas"   ==  "l_ c_ader_"
--    sustituye "as" "es" "las casaderas"  ==  "les cesaderes"
--    sustituye "asa" "a" "las casaderas"  ==  "las caderas"
--    sustituye "asd" "a" "las casaderas"  ==  "las casaderas"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Sustitucion_de_listas where

import Data.List (isPrefixOf, intercalate, stripPrefix)
import Data.List.Split (splitOn)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sustituye1 :: Eq a => [a] -> [a] -> [a] -> [a]
sustituye1 _  _ [] = []
sustituye1 xs ys zs@(z:zs')
  | xs `isPrefixOf` zs = ys ++ sustituye1 xs ys (drop (length xs) zs)
  | otherwise          = z : sustituye1 xs ys zs'

-- 2ª solución
-- ===========

sustituye2 :: Eq a => [a] -> [a] -> [a] -> [a]
sustituye2 _ _ [] = []
sustituye2 xs ys zs@(z:zs') =
  case stripPrefix xs zs of
    Just resto -> ys ++ sustituye2 xs ys resto
    Nothing    -> z : sustituye2 xs ys zs'


-- 3ª solución
-- ===========

sustituye3 :: Eq a => [a] -> [a] -> [a] -> [a]
sustituye3 _ _ [] = []
sustituye3 xs ys zs@(z:zs') =
  maybe (z : sustituye3 xs ys zs')
        (\resto -> ys ++ sustituye3 xs ys resto)
        (stripPrefix xs zs)

-- 4ª solución
-- ===========

sustituye4 :: Eq a => [a] -> [a] -> [a] -> [a]
sustituye4 xs ys = intercalate ys . splitOn xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String -> String -> String) -> Spec
specG sustituye = do
  it "e1" $
    sustituye "as" "_" "las casaderas"  `shouldBe` "l_ c_ader_"
  it "e2" $
    sustituye "as" "es" "las casaderas" `shouldBe` "les cesaderes"
  it "e3" $
    sustituye "asa" "a" "las casaderas" `shouldBe` "las caderas"
  it "e4" $
    sustituye "asd" "a" "las casaderas" `shouldBe` "las casaderas"

spec :: Spec
spec = do
  describe "def. 1" $ specG sustituye1
  describe "def. 2" $ specG sustituye2
  describe "def. 3" $ specG sustituye3
  describe "def. 4" $ specG sustituye4

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- -- La propiedad es
prop_equivalencia :: NonEmptyList Char -> String -> String -> Bool
prop_equivalencia (NonEmpty xs) ys zs =
  all (== sustituye1 xs ys zs)
      [sustituye2 xs ys zs,
       sustituye3 xs ys zs,
       sustituye4 xs ys zs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (sustituye1 "123" "" (take (10^7) (cycle "1234")))
--    2500000
--    (2.39 secs, 1,920,602,240 bytes)
--    λ> length (sustituye2 "123" "" (take (10^7) (cycle "1234")))
--    2500000
--    (2.06 secs, 2,080,602,216 bytes)
--    λ> length (sustituye3 "123" "" (take (10^7) (cycle "1234")))
--    2500000
--    (2.56 secs, 2,340,603,528 bytes)
--    λ> length (sustituye4 "123" "" (take (10^7) (cycle "1234")))
--    2500000
--    (0.56 secs, 2,940,603,136 bytes)
