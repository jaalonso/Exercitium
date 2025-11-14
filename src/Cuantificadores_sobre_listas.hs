-- Cuantificadores_sobre_listas.hs
-- Cuantificadores sobre listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-Noviembre-2014 (actualizado 14-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    verificaP :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaP p xs) se verifica si cada elemento de la lista xss
-- contiene algún elemento que cumple el predicado p. Por ejemplo,
--    verificaP odd [[1,3,4,2], [4,5], [9]] == True
--    verificaP odd [[1,3,4,2], [4,8], [9]] == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cuantificadores_sobre_listas where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

verificaP1 :: (a -> Bool) -> [[a]] -> Bool
verificaP1 p xss = and [any p xs | xs <- xss]

-- 2ª solución
-- ===========

verificaP2 :: (a -> Bool) -> [[a]] -> Bool
verificaP2 _ []       = True
verificaP2 p (xs:xss) = any p xs && verificaP2 p xss

-- 3ª solución
-- ===========

verificaP3 :: (a -> Bool) -> [[a]] -> Bool
verificaP3 p = foldr ((&&) . any p) True

-- 4ª solución
-- ===========

verificaP4 :: (a -> Bool) -> [[a]] -> Bool
verificaP4 p = all (any p)

-- 5ª solución
-- ===========

verificaP5 :: (a -> Bool) -> [[a]] -> Bool
verificaP5 = all . any

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int -> Bool) -> [[Int]] -> Bool) -> Spec
specG verificaP = do
  it "e1" $
    verificaP odd [[1,3,4,2], [4,5], [9]] `shouldBe` True
  it "e2" $
    verificaP odd [[1,3,4,2], [4,8], [9]] `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG verificaP1
  describe "def. 2" $ specG verificaP2
  describe "def. 3" $ specG verificaP3
  describe "def. 4" $ specG verificaP4
  describe "def. 5" $ specG verificaP5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: (Int -> Bool) -> [[Int]] -> Bool
prop_equivalencia p xss =
  all (== verificaP1 p xss)
      [verificaP2 p xss,
       verificaP3 p xss,
       verificaP4 p xss,
       verificaP5 p xss]

-- La comprobación es
--    λ> quickCheck' prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> verificaP1 even [[x,x+2..x+10] ++ [0] | x <- [1,3..3000001]]
--    True
--    (2.11 secs, 4,308,601,216 bytes)
--    λ> verificaP2 even [[x,x+2..x+10] ++ [0] | x <- [1,3..3000001]]
--    True
--    (2.32 secs, 4,224,602,000 bytes)
--    λ> verificaP3 even [[x,x+2..x+10] ++ [0] | x <- [1,3..3000001]]
--    True
--    (1.87 secs, 4,080,601,088 bytes)
--    λ> verificaP4 even [[x,x+2..x+10] ++ [0] | x <- [1,3..3000001]]
--    True
--    (1.81 secs, 4,068,601,008 bytes)
--    λ> verificaP5 even [[x,x+2..x+10] ++ [0] | x <- [1,3..3000001]]
--    True
--    (1.76 secs, 4,068,601,152 bytes)
