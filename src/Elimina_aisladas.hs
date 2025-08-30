-- Elimina_aisladas.hs
-- Eliminación de las ocurrencias aisladas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-Junio-2014 (actualizado 30-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    eliminaAisladas :: Eq a => a -> [a] -> [a]
-- tal que (eliminaAisladas x ys) es la lista obtenida eliminando de ys
-- las ocurrencias aisladas de x (es decir, aquellas ocurrencias de x
-- tales que su elemento anterior y posterior son distintos de x). Por
-- ejemplo,
--    eliminaAisladas 'X' ""                  == ""
--    eliminaAisladas 'X' "X"                 == ""
--    eliminaAisladas 'X' "XX"                == "XX"
--    eliminaAisladas 'X' "XXX"               == "XXX"
--    eliminaAisladas 'X' "abcd"              == "abcd"
--    eliminaAisladas 'X' "Xabcd"             == "abcd"
--    eliminaAisladas 'X' "XXabcd"            == "XXabcd"
--    eliminaAisladas 'X' "XXXabcd"           == "XXXabcd"
--    eliminaAisladas 'X' "abcdX"             == "abcd"
--    eliminaAisladas 'X' "abcdXX"            == "abcdXX"
--    eliminaAisladas 'X' "abcdXXX"           == "abcdXXX"
--    eliminaAisladas 'X' "abXcd"             == "abcd"
--    eliminaAisladas 'X' "abXXcd"            == "abXXcd"
--    eliminaAisladas 'X' "abXXXcd"           == "abXXXcd"
--    eliminaAisladas 'X' "XabXcdX"           == "abcd"
--    eliminaAisladas 'X' "XXabXXcdXX"        == "XXabXXcdXX"
--    eliminaAisladas 'X' "XXXabXXXcdXXX"     == "XXXabXXXcdXXX"
--    eliminaAisladas 'X' "XabXXcdXeXXXfXx"   == "abXXcdeXXXfx"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elimina_aisladas where

import Data.List (group)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

eliminaAisladas1 :: Eq a => a -> [a] -> [a]
eliminaAisladas1 _ [] = []
eliminaAisladas1 x [y]
  | x == y    = []
  | otherwise = [y]
eliminaAisladas1 x (y1:y2:ys)
  | y1 /= x   = y1 : eliminaAisladas1 x (y2:ys)
  | y2 /= x   = y2 : eliminaAisladas1 x ys
  | otherwise = takeWhile (==x) (y1:y2:ys) ++
                eliminaAisladas1 x (dropWhile (==x) ys)

-- 2ª solución
-- ===========

eliminaAisladas2 :: Eq a => a -> [a] -> [a]
eliminaAisladas2 _ [] = []
eliminaAisladas2 x ys
  | cs == [x] = as ++ eliminaAisladas2 x ds
  | otherwise = as ++ cs ++ eliminaAisladas2 x ds
  where (as,bs) = span (/=x) ys
        (cs,ds) = span (==x) bs

-- 3ª solución
-- ===========

eliminaAisladas3 :: Eq a => a -> [a] -> [a]
eliminaAisladas3 x ys =
  concat [zs | zs <- group ys, zs /= [x]]

-- 4ª solución
-- ===========

eliminaAisladas4 :: Eq a => a -> [a] -> [a]
eliminaAisladas4 x =
  concat . filter (/= [x]) . group

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Char -> String -> String) -> Spec
specG eliminaAisladas = do
  it "e1" $
    eliminaAisladas 'X' ""                `shouldBe` ""
  it "e2" $
    eliminaAisladas 'X' "X"               `shouldBe` ""
  it "e3" $
    eliminaAisladas 'X' "XX"              `shouldBe` "XX"
  it "e4" $
    eliminaAisladas 'X' "XXX"             `shouldBe` "XXX"
  it "e5" $
    eliminaAisladas 'X' "abcd"            `shouldBe` "abcd"
  it "e6" $
    eliminaAisladas 'X' "Xabcd"           `shouldBe` "abcd"
  it "e7" $
    eliminaAisladas 'X' "XXabcd"          `shouldBe` "XXabcd"
  it "e8" $
    eliminaAisladas 'X' "XXXabcd"         `shouldBe` "XXXabcd"
  it "e9" $
    eliminaAisladas 'X' "abcdX"           `shouldBe` "abcd"
  it "e10" $
    eliminaAisladas 'X' "abcdXX"          `shouldBe` "abcdXX"
  it "e11" $
    eliminaAisladas 'X' "abcdXXX"         `shouldBe` "abcdXXX"
  it "e12" $
    eliminaAisladas 'X' "abXcd"           `shouldBe` "abcd"
  it "e13" $
    eliminaAisladas 'X' "abXXcd"          `shouldBe` "abXXcd"
  it "e14" $
    eliminaAisladas 'X' "abXXXcd"         `shouldBe` "abXXXcd"
  it "e15" $
    eliminaAisladas 'X' "XabXcdX"         `shouldBe` "abcd"
  it "e16" $
    eliminaAisladas 'X' "XXabXXcdXX"      `shouldBe` "XXabXXcdXX"
  it "e17" $
    eliminaAisladas 'X' "XXXabXXXcdXXX"   `shouldBe` "XXXabXXXcdXXX"
  it "e18" $
    eliminaAisladas 'X' "XabXXcdXeXXXfXx" `shouldBe` "abXXcdeXXXfx"

spec :: Spec
spec = do
  describe "def. 1"  $ specG eliminaAisladas1
  describe "def. 2"  $ specG eliminaAisladas2
  describe "def. 3"  $ specG eliminaAisladas3
  describe "def. 4"  $ specG eliminaAisladas4

-- La verificación es
--    λ> verifica
--    72 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_eliminaAisladas :: Int -> [Int] -> Bool
prop_eliminaAisladas x ys =
  all (== eliminaAisladas1 x ys)
      [eliminaAisladas2 x ys,
       eliminaAisladas3 x ys,
       eliminaAisladas4 x ys]

-- La comprobación es
--    λ> quickCheck prop_eliminaAisladas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (eliminaAisladas1 'a' (take (5*10^6) (cycle "abca")))
--    4999998
--    (3.86 secs, 2,030,515,400 bytes)
--    λ> length (eliminaAisladas2 'a' (take (5*10^6) (cycle "abca")))
--    4999998
--    (3.41 secs, 2,210,516,832 bytes)
--    λ> length (eliminaAisladas3 'a' (take (5*10^6) (cycle "abca")))
--    4999998
--    (2.11 secs, 2,280,516,448 bytes)
--    λ> length (eliminaAisladas4 'a' (take (5*10^6) (cycle "abca")))
--    4999998
--    (0.92 secs, 1,920,516,704 bytes)
