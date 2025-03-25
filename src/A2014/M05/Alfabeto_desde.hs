-- Alfabeto_desde.hs
-- Alfabeto comenzando en un carácter.
-- José A. Alonso <https://jaalonso.github.io>
-- Sevilla, 12-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    alfabetoDesde :: Char -> String
-- tal que (alfabetoDesde c) es el alfabeto, en minúscula, comenzando en
-- el carácter c, si c es una letra minúscula y comenzando en 'a', en
-- caso contrario. Por ejemplo,
--    alfabetoDesde 'e'  ==  "efghijklmnopqrstuvwxyzabcd"
--    alfabetoDesde 'a'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde '7'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde '{'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde 'B'  ==  "abcdefghijklmnopqrstuvwxyz"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module A2014.M05.Alfabeto_desde where

import Data.Char (isLower, isAscii)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

alfabetoDesde1 :: Char -> String
alfabetoDesde1 c =
  dropWhile (<c) ['a'..'z'] ++ takeWhile (<c) ['a'..'z']

-- 2ª solución
-- ===========

alfabetoDesde2 :: Char -> String
alfabetoDesde2 c = ys ++ xs
  where (xs,ys) = span (<c) ['a'..'z']

-- 3ª solución
-- ===========

alfabetoDesde3 :: Char -> String
alfabetoDesde3 c = ys ++ xs
  where (xs,ys) = break (==c) ['a'..'z']

-- 4ª solución
-- ===========

alfabetoDesde4 :: Char -> String
alfabetoDesde4 c
  | 'a' <= c && c <= 'z' = [c..'z'] ++ ['a'..pred c]
  | otherwise            = ['a'..'z']

-- 5ª solución
-- ===========

alfabetoDesde5 :: Char -> String
alfabetoDesde5 c
  | isLower c = [c..'z'] ++ ['a'..pred c]
  | otherwise = ['a'..'z']

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Char -> String) -> Spec
specG alfabetoDesde = do
  it "e1" $
    alfabetoDesde 'e'  `shouldBe`  "efghijklmnopqrstuvwxyzabcd"
  it "e2" $
    alfabetoDesde 'a'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e3" $
    alfabetoDesde '7'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e4" $
    alfabetoDesde '{'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e5" $
    alfabetoDesde 'B'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"

spec :: Spec
spec = do
  describe "def. 1" $ specG alfabetoDesde1
  describe "def. 2" $ specG alfabetoDesde2
  describe "def. 3" $ specG alfabetoDesde3
  describe "def. 4" $ specG alfabetoDesde4
  describe "def. 4" $ specG alfabetoDesde5

-- La verificación es
--    λ> verifica
--    25 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_alfabetoDesde :: Property
prop_alfabetoDesde =
  forAll (arbitrary `suchThat` isAscii) $ \c ->
  all (== alfabetoDesde1 c)
      [f c | f <- [alfabetoDesde2,
                   alfabetoDesde3,
                   alfabetoDesde4,
                   alfabetoDesde5]]


-- La comprobación es
--    λ> quickCheck prop_alfabetoDesde
--    +++ OK, passed 100 tests.
