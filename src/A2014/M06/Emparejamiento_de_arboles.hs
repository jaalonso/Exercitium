-- Emparejamiento_de_arboles.hs
-- Emparejamiento de árboles.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-junio-2014
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

module A2014.M06.Emparejamiento_de_arboles where

import Data.Tree (Tree (..))
import Control.Monad.Zip (mzipWith)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen,
                        arbitrary, generate, sublistOf, sized, quickCheck)

data Arbol a = N a [Arbol a]
  deriving (Show, Eq)

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
-- ===========

emparejaArboles1 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles1 f (N x xs) (N y ys) =
  N (f x y) (emparejaListaArboles f xs ys)

emparejaListaArboles :: (a -> b -> c) -> [Arbol a] -> [Arbol b] -> [Arbol c]
emparejaListaArboles _ [] _ = []
emparejaListaArboles _ _ [] = []
emparejaListaArboles f (x:xs) (y:ys) =
  emparejaArboles1 f x y : emparejaListaArboles f xs ys

-- 2ª solución
-- ===========

emparejaArboles2 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles2 f (N x xs) (N y ys) =
  N (f x y) (zipWith (emparejaArboles2 f) xs ys)

-- 3ª solución
-- ===========

emparejaArboles3 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles3 f x y =
  treeAarbol (mzipWith f (arbolAtree x) (arbolAtree y))

arbolAtree :: Arbol a -> Tree a
arbolAtree (N x xs) = Node x (map arbolAtree xs)

treeAarbol :: Tree a -> Arbol a
treeAarbol (Node x xs) = N x (map treeAarbol xs)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int -> Int -> Int) -> Arbol Int -> Arbol Int -> Arbol Int) -> Spec
specG emparejaArboles = do
  it "e1" $
    show (emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []]))
      `shouldBe` "N 2 [N 8 []]"
  it "e2" $
    show (emparejaArboles (+) ej1 ej2)
      `shouldBe` "N 4 [N 11 [],N 7 []]"
  it "e3" $
    show (emparejaArboles (+) ej1 ej1)
      `shouldBe` "N 2 [N 12 [],N 6 [N 10 []]]"

spec :: Spec
spec = do
  describe "def. 1"  $ specG emparejaArboles1
  describe "def. 2"  $ specG emparejaArboles2
  describe "def. 3"  $ specG emparejaArboles3

-- La verificación es
--    λ> verifica
--    9 examples, 0 failures

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
  emparejaArboles1 (*) x y == emparejaArboles2 (*) x y &&
  emparejaArboles1 (+) x y == emparejaArboles3 (+) x y &&
  emparejaArboles1 (*) x y == emparejaArboles3 (*) x y

-- La comprobación es
--    λ> quickCheck prop_emparejaArboles
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> a500 <- generate (arbolArbitrario 500 :: Gen (Arbol Int))
--    λ> emparejaArboles1 (+) a500 a500 == emparejaArboles1 (+) a500 a500
--    True
--    (3.03 secs, 1,981,353,912 bytes)
--    λ> emparejaArboles2 (+) a500 a500 == emparejaArboles1 (+) a500 a500
--    True
--    (2.12 secs, 1,325,826,688 bytes)
--    λ> emparejaArboles3 (+) a500 a500 == emparejaArboles1 (+) a500 a500
--    True
--    (2.57 secs, 1,937,547,296 bytes)
