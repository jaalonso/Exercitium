-- Arboles_binarios.hs
-- El tipo de los árboles binarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-diciembre-2022
-- ---------------------------------------------------------------------

-- El árbol binario
--         9
--        / \
--       /   \
--      3     7
--     / \
--    2   4
-- se puede representar por
--    N 9 (N 3 (H 2) (H 4)) (H 7)
-- usando el tipo de los árboles binarios definido como se muestra a
-- continuación.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Arboles_binarios where

import Test.QuickCheck

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- Generador de árboles binarios
-- =============================

-- (arbolArbitrario n) es un árbol aleatorio de altura n. Por ejemplo,
--    λ> sample (arbolArbitrario 3 :: Gen (Arbol Int))
--    N 0 (H 0) (H 0)
--    N 1 (N (-2) (H (-1)) (H 1)) (H 2)
--    N 3 (H 1) (H 2)
--    N 6 (N 0 (H 5) (H (-5))) (N (-5) (H (-5)) (H 4))
--    H 7
--    N (-8) (H (-8)) (H 9)
--    H 2
--    N (-1) (H 7) (N 9 (H (-2)) (H (-8)))
--    H (-3)
--    N 0 (N 16 (H (-14)) (H (-18))) (H 7)
--    N (-16) (H 18) (N (-19) (H (-15)) (H (-18)))
arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario 0 = H <$> arbitrary
arbolArbitrario n =
  oneof [H <$> arbitrary,
         N <$> arbitrary <*> arbolArbitrario (div n 2) <*> arbolArbitrario (div n 2)]

-- Arbol es subclase de Arbitrary
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario
