-- Emparejamiento_de_arboles.hs
-- Emparejamiento de árboles.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--      deriving (Show, Eq)
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\
--    6   3           / | \
--        |          5  4  7
--        5          |     /\
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 6 [],N 3 [N 5 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
--
-- Definir la función
--    emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
-- tal que (emparejaArboles f a1 a2) es el árbol obtenido aplicando la
-- función f a los elementos de los árboles a1 y a2 que se encuentran en
-- la misma posición. Por ejemplo,
--    λ> emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
--    N 2 [N 8 []]
--    λ> emparejaArboles (+) ej1 ej2
--    N 4 [N 11 [],N 7 []]
--    λ> emparejaArboles (+) ej1 ej1
--    N 2 [N 12 [],N 6 [N 10 []]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Emparejamiento_de_arboles where

import Data.Tree (Tree (..))
import Control.Monad.Zip (mzipWith)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, sublistOf, sized, quickCheck)

data Arbol a = N a [Arbol a]
  deriving (Show, Eq)

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
-- ===========

emparejaArboles1 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles1 f (N x xs) (N y ys) =
  N (f x y) (zipWith (emparejaArboles1 f) xs ys)

-- 2ª solución
-- ===========

emparejaArboles2 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles2 f x y =
  treeAarbol (mzipWith f (arbolAtree x) (arbolAtree y))

arbolAtree :: Arbol a -> Tree a
arbolAtree (N x xs) = Node x (map arbolAtree xs)

treeAarbol :: Tree a -> Arbol a
treeAarbol (Node x xs) = N x (map treeAarbol xs)

-- Comprobación de equivalencia
-- ============================

-- (arbolArbitrario n) es un árbol aleatorio de orden n. Por ejemplo,
--    λ> generate (arbolArbitrario 5 :: Gen (Arbol Int))
--    N (-26) [N 8 [N 6 [N 11 []]],N 7 []]
--    λ> generate (arbolArbitrario 5 :: Gen (Arbol Int))
--    N 1 []
--    λ> generate (arbolArbitrario 5 :: Gen (Arbol Int))
--    N (-19) [N (-11) [],N 25 [],N 19 [N (-27) [],N (-19) [N 17 []]]]
arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario n = do
  x  <- arbitrary
  ms <- sublistOf [0 .. n `div` 2]
  as <- mapM arbolArbitrario ms
  return (N x as)

-- Arbol es una subclase de Arbitraria
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario

-- La propiedad es
prop_emparejaArboles :: Arbol Int -> Arbol Int -> Bool
prop_emparejaArboles x y =
  emparejaArboles1 (+) x y == emparejaArboles2 (+) x y &&
  emparejaArboles1 (*) x y == emparejaArboles2 (*) x y

-- La comprobación es
--    λ> quickCheck prop_emparejaArboles
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> emparejaArboles1 (+) a500 a500 == emparejaArboles1 (+) a500 a500
--    True
--    (4.88 secs, 3,777,059,712 bytes)
--    λ> emparejaArboles2 (+) a500 a500 == emparejaArboles2 (+) a500 a500
--    True
--    (2.73 secs, 2,315,142,248 bytes)
