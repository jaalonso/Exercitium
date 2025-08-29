-- Enumera_arbol.hs
-- Enumeración de árboles binarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-Mayo-2014 (actualizado 29-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles binarios se pueden representar mediante el tipo Arbol
-- definido por
--    data Arbol a = H a
--                 | N (Arbol a) a (Arbol a)
--       deriving Show
-- Por ejemplo, el árbol
--         "B"
--         / \
--        /   \
--       /     \
--     "B"     "A"
--     / \     / \
--   "A" "B" "C" "C"
-- se puede definir por
--    ej1 :: Arbol String
--    ej1 = N (N (H "A") "B" (H "B")) "B" (N (H "C") "A" (H "C"))
--
-- Definir la función
--    enumeraArbol :: Arbol t -> Arbol Int
-- tal que (enumeraArbol a) es el árbol obtenido numerando las hojas y
-- los nodos de a desde la hoja izquierda hasta la raíz. Por ejemplo,
--    λ> enumeraArbol ej1
--    N (N (H 0) 1 (H 2)) 3 (N (H 4) 5 (H 6))
-- Gráficamente,
--          3
--         / \
--        /   \
--       /     \
--      1       5
--     / \     / \
--    0   2   4   6
-- ---------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Enumera_arbol where

import Control.Monad.State (State, evalState, get, modify)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, quickCheck, sized)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

data Arbol a = H a
             | N (Arbol a) a (Arbol a)
  deriving (Show, Eq, Foldable, Functor, Traversable)

ej1 :: Arbol String
ej1 = N (N (H "A") "B" (H "B")) "B" (N (H "C") "A" (H "C"))

-- 1ª solución
-- ===========

enumeraArbol1 :: Arbol t -> Arbol Int
enumeraArbol1 a = fst (aux a 0)
  where
    aux :: Arbol a -> Int -> (Arbol Int,Int)
    aux (H _) n     = (H n, n+1)
    aux (N i _ d) n = (N i' n1 d', n2)
      where (i', n1) = aux i n
            (d', n2) = aux d (n1+1)

-- 2ª solución
-- ===========

enumeraArbol2 :: Arbol t -> Arbol Int
enumeraArbol2 a = evalState (aux a) 0
  where
    aux :: Arbol t -> State Int (Arbol Int)
    aux (H _)     = H <$> enumeraNodo
    aux (N i _ d) = do
      i' <- aux i
      n  <- enumeraNodo
      d' <- aux d
      return (N i' n d')

enumeraNodo :: State Int Int
enumeraNodo = do
  n <- get
  modify succ
  return n

-- 3ª solución
-- ===========

enumeraArbol3 :: Arbol t -> Arbol Int
enumeraArbol3 a = evalState (aux a) 0
  where
    aux :: Arbol t -> State Int (Arbol Int)
    aux (H _)     = H <$> enumeraNodo
    aux (N i _ d) = N <$> aux i <*> enumeraNodo <*> aux d

-- 4ª solución
-- ===========

enumeraArbol4 :: Arbol t -> Arbol Int
enumeraArbol4 a = evalState (traverse enumeraNodo4 a) 0
  where
    enumeraNodo4 :: t -> State Int Int
    enumeraNodo4 _ = do
      n <- get
      modify succ
      return n

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Arbol String -> Arbol Int) -> Spec
specG enumeraArbol = do
  it "e1" $
    enumeraArbol ej1
    `shouldBe` N (N (H 0) 1 (H 2)) 3 (N (H 4) 5 (H 6))

spec :: Spec
spec = do
  describe "def. 1" $ specG enumeraArbol1
  describe "def. 2" $ specG enumeraArbol2
  describe "def. 3" $ specG enumeraArbol3
  describe "def. 4" $ specG enumeraArbol4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- (arbolArbitrario n) genera un árbol aleatorio de orden n. Por
-- ejemplo,
--    λ> generate (arbolArbitrario 3 :: Gen (Arbol Int))
--    N (N (H 19) 0 (H (-27))) 21 (N (H 2) 17 (H 26))
arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario n | n <= 0    = H <$> arbitrary
                  | otherwise = N <$> subarbol <*> arbitrary <*> subarbol
  where subarbol = arbolArbitrario (n `div` 2)

-- Arbol es una subclase de Arbitrary.
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario

-- La propiedad es
prop_enumeraArbol :: Arbol Int -> Bool
prop_enumeraArbol a =
  all (== enumeraArbol1 a)
      [enumeraArbol2 a,
       enumeraArbol3 a,
       enumeraArbol4 a]

-- La comprobación es
--    λ> quickCheck prop_enumeraArbol
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- (arbol n) es el árbol completo de profundidad n. Por ejemplo,
--   λ> arbol 2
--   N (N (H 0) 0 (H 0)) 0 (N (H 0) 0 (H 0))
arbol :: Int -> Arbol Int
arbol 0 = H 0
arbol n = N (arbol (n-1)) 0 (arbol (n-1))

-- (maximo a) es el máximo de los elementos de a. Por ejemplo,
--    maximo ej1  ==  "C"
maximo :: Ord a => Arbol a -> a
maximo (H x) = x
maximo (N i x d) = maximum [maximo i, x, maximo d]

-- La comparación es
--    λ> maximo (enumeraArbol1 (arbol 19))
--    1048574
--    (1.22 secs, 755,475,496 bytes)
--    λ> maximo (enumeraArbol2 (arbol 19))
--    1048574
--    (2.21 secs, 1,644,666,792 bytes)
--    λ> maximo (enumeraArbol3 (arbol 19))
--    1048574
--    (2.44 secs, 1,799,855,984 bytes)
--    λ> maximo (enumeraArbol4 (arbol 19))
--    1048574
--    (2.89 secs, 1,753,719,616 bytes)
