-- Renombra_arbol.hs
-- Renombramiento de un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-Julio-2014 (actualizado 9-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles binarios se pueden representar mediante el tipo Arbol
-- definido por
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--      deriving (Show, Eq, Foldable, Functor)
-- Por ejemplo, el árbol
--         "C"
--         / \
--        /   \
--       /     \
--     "B"     "A"
--     / \     / \
--   "A" "B" "B" "C"
-- se puede definir por
--    ej1 :: Arbol String
--    ej1 = N "C" (N "B" (H "A") (H "B")) (N "A" (H "B") (H "C"))
--
-- Definir la función
--    renombraArbol :: Arbol t -> Arbol Int
-- tal que (renombraArbol a) es el árbol obtenido sustituyendo el valor
-- de los nodos y hojas por números tales que tengan el mismo valor si y
-- sólo si coincide su contenido. Por ejemplo,
--    λ> renombraArbol ej1
--    N 2 (N 1 (H 0) (H 1)) (N 0 (H 1) (H 2))
-- Gráficamente,
--          2
--         / \
--        /   \
--       /     \
--      1       0
--     / \     / \
--    0   1   1   2
--
-- Nótese que los elementos del árbol pueden ser de cualquier tipo. Por
-- ejemplo,
--    λ> renombraArbol (N 9 (N 4 (H 8) (H 4)) (N 8 (H 4) (H 9)))
--    N 2 (N 0 (H 1) (H 0)) (N 1 (H 0) (H 2))
--    λ> renombraArbol (N True (N False (H True) (H False)) (H True))
--    N 1 (N 0 (H 1) (H 0)) (H 1)
--    λ> renombraArbol (N False (N False (H True) (H False)) (H True))
--    N 0 (N 0 (H 1) (H 0)) (H 1)
--    λ> renombraArbol (H False)
--    H 0
--    λ> renombraArbol (H True)
--    H 0
-- ---------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Renombra_arbol where

import Data.Map.Strict (Map, (!), fromList)
import Data.List (nub, sort, elemIndex)
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq, Foldable, Functor)

ej1 :: Arbol String
ej1 = N "C" (N "B" (H "A") (H "B")) (N "A" (H "B") (H "C"))

-- 1ª solución
-- ===========

renombraArbol1 :: Ord t => Arbol t -> Arbol Int
renombraArbol1 a = aux a
  where ys            = valores a
        aux (H x)     = H (posicion x ys)
        aux (N x i d) = N (posicion x ys) (aux i) (aux d)

-- (valores a) es la lista de los valores en los nodos y las hojas del
-- árbol a. Por ejemplo,
--    valores ej1  ==  ["A","B","C"]
valores :: Ord a => Arbol a -> [a]
valores a = sort (nub (aux a))
    where aux (H x)     = [x]
          aux (N x i d) = x : (aux i ++ aux d)

-- (posicion x ys) es la posición de x en ys. Por ejemplo.
--    posicion 7 [5,3,7,8]  ==  2
posicion :: Ord a => a -> [a] -> Int
posicion x ys =
  head [n | (y,n) <- zip ys [0..], y == x]

-- 2ª solución
-- ===========

renombraArbol2 :: Ord t => Arbol t -> Arbol Int
renombraArbol2 a = aux a
  where ys = valores a
        aux (H x)     = H (posicion2 x ys)
        aux (N x i d) = N (posicion2 x ys) (aux i) (aux d)

posicion2 :: Ord a => a -> [a] -> Int
posicion2 x ys =
  fromJust (elemIndex x ys)

-- 3ª solución
-- ===========

renombraArbol3 :: Ord t => Arbol t -> Arbol Int
renombraArbol3 a = aux a
  where
    ys = sort (nub (toList a))
    aux (H x)     = H (fromJust (elemIndex x ys))
    aux (N x i d) = N (fromJust (elemIndex x ys)) (aux i) (aux d)

-- 4ª solución
-- ===========

renombraArbol4 :: Ord t => Arbol t -> Arbol Int
renombraArbol4 a = fmap convertir a
  where
    indice = zip (sort (nub (toList a))) [0..]
    convertir x = fromJust (lookup x indice)

-- 5ª solución
-- ===========

-- (dicValores a) es el diccionario de los valores en los nodos y las
-- hojas del árbol a. Por ejemplo,
--    λ> dicValores ej1
--    fromList [("A",0),("B",1),("C",2)]
dicValores :: Ord a => Arbol a -> Map a Int
dicValores a =
  fromList $ zip (valores a) [0..]

renombraArbol5 :: Ord t => Arbol t -> Arbol Int
renombraArbol5 a =
  repl a
  where
    dic = dicValores a
    repl (H x)     = H (dic ! x)
    repl (N x i d) = N (dic ! x) (repl i) (repl d)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Arbol String -> Arbol Int) -> Spec
specG renombraArbol = do
  it "e1" $
    show (renombraArbol ej1) `shouldBe`
    "N 2 (N 1 (H 0) (H 1)) (N 0 (H 1) (H 2))"

spec :: Spec
spec = do
  describe "def. 1" $ specG renombraArbol1
  describe "def. 2" $ specG renombraArbol2
  describe "def. 3" $ specG renombraArbol3
  describe "def. 4" $ specG renombraArbol4
  describe "def. 5" $ specG renombraArbol5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- (genArbol n) genera un árbol aleatorio de orden n. Por ejemplo,
--    λ> generate (genArbol 3 :: Gen (Arbol Int))
--    N (-13) (H 1) (N 30 (H (-10)) (H (-1)))
--    λ> generate (genArbol 3 :: Gen (Arbol Int))
--    N (-3) (H (-29)) (N (-17) (H 8) (H 28))
genArbol :: Arbitrary a => Int -> Gen (Arbol a)
genArbol 0 = H <$> arbitrary
genArbol n = frequency
  [ (1, H <$> arbitrary)
  , (3, N <$> arbitrary <*> sub <*> sub)]
  where
    sub = genArbol (n `div` 2)

-- Arbol es subclase de Arbitraria
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized genArbol

-- La propiedad es
prop_renombraArbol :: Arbol Int -> Bool
prop_renombraArbol a =
  all (== renombraArbol1 a)
      [renombraArbol2 a,
       renombraArbol3 a,
       renombraArbol4 a,
       renombraArbol5 a]

-- La comprobación es
--    λ> quickCheck prop_renombraArbol
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- (arbol n) es el árbol completo de profundidad n. Por ejemplo,
--    λ> arbol 2
--    N 12 (N 11 (H 0) (H 0)) (N 11 (H 0) (H 0))
--    λ> renombraArbol1 (arbol 2)
--    N 2 (N 1 (H 0) (H 0)) (N 1 (H 0) (H 0))
arbol :: Int -> Arbol Int
arbol 0 = H 0
arbol n = N (n+10) (arbol (n-1)) (arbol (n-1))

-- La comparación es
--    λ> length (renombraArbol1 (arbol 20))
--    2097151
--    (2.13 secs, 1,191,782,032 bytes)
--    λ> length (renombraArbol2 (arbol 20))
--    2097151
--    (2.26 secs, 1,191,782,056 bytes)
--    λ> length (renombraArbol3 (arbol 20))
--    2097151
--    (2.16 secs, 1,225,336,576 bytes)
--    λ> length (renombraArbol4 (arbol 20))
--    2097151
--    (1.96 secs, 1,032,398,632 bytes)
--    λ> length (renombraArbol5 (arbol 20))
--    2097151
--    (2.00 secs, 1,191,782,056 bytes)
