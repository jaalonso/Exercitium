-- Numero_de_hojas_de_un_arbol_binario.hs
-- Número de hojas y de nodos de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-diciembre-2022
-- ---------------------------------------------------------------------

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
--
-- El tipo de los árboles binarios se puede definir por
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--      deriving (Show, Eq)
--
-- Definir las funciones
--    nHojas :: Arbol a -> Int
--    nNodos :: Arbol a -> Int
-- tales que
-- + (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--      nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- + (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--      nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
--
-- Comprobar con QuickCheck que en todo árbol binario el número de sus
-- hojas es igual al número de sus nodos más uno.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numero_de_hojas_de_un_arbol_binario where

import Test.QuickCheck

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N _ i d) = nHojas i + nHojas d

nNodos :: Arbol a -> Int
nNodos (H _)     = 0
nNodos (N _ i d) = 1 + nNodos i + nNodos d

-- Comprobación de equivalencia
-- ============================

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

-- La propiedad es
prop_nHojas :: Arbol Int -> Bool
prop_nHojas x =
  nHojas x == nNodos x + 1

-- La comprobación es
--    λ> quickCheck prop_nHojas
--    OK, passed 100 tests.
