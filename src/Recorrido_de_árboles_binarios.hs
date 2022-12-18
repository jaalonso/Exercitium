-- Recorrido_de_árboles_binarios.hs
-- Recorrido de árboles binarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-diciembre-2022
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
--    preorden  :: Arbol a -> [a]
--    postorden :: Arbol a -> [a]
-- tales que
-- + (preorden x) es la lista correspondiente al recorrido preorden del
--   árbol x; es decir, primero visita la raíz del árbol, a continuación
--   recorre el subárbol izquierdo y, finalmente, recorre el subárbol
--   derecho. Por ejemplo,
--      preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- + (postorden x) es la lista correspondiente al recorrido postorden
--   del árbol x; es decir, primero recorre el subárbol izquierdo, a
--   continuación el subárbol derecho y, finalmente, la raíz del
--   árbol. Por ejemplo,
--      postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
--
-- Comprobar con QuickCheck que la longitud de la lista
-- obtenida recorriendo un árbol en cualquiera de los sentidos es igual
-- al número de nodos del árbol más el número de hojas.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Recorrido_de_árboles_binarios where

import Test.QuickCheck

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- 1ª definición de preorden
-- =========================

preorden1 :: Arbol a -> [a]
preorden1 (H x)     = [x]
preorden1 (N x i d) = x : preorden1 i ++ preorden1 d

-- 2ª definición de preorden
-- =========================

preorden2 :: Arbol a -> [a]
preorden2 = aux []
  where aux xs (H x)     = x : xs
        aux xs (N x i d) = x : aux (aux xs d) i

-- Comprobación de equivalencia de preorden
-- ========================================

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
prop_preorden :: Arbol Int -> Bool
prop_preorden x =
  preorden1 x == preorden2 x

-- La comprobación es
--    λ> quickCheck prop_preorden
--    OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- (arbol n) es el arbol de profundidad n con sus valores iguales a
-- 1. Por ejemplo,
--    arbol 2  ==  N 1 (N 1 (H 1) (H 1)) (N 1 (H 1) (H 1))
arbol :: Int -> Arbol Int
arbol 0 = H 1
arbol n = N 1 a a
  where a = arbol (n-1)

-- La comparación es
--    λ> length (preorden1 (arbol 22))
--    8388607
--    (3.40 secs, 6,073,946,816 bytes)
--    λ> length (preorden2 (arbol 22))
--    8388607
--    (1.81 secs, 1,107,890,872 bytes)

-- 1ª definición de postorden
-- ==========================

postorden1 :: Arbol a -> [a]
postorden1 (H x)     = [x]
postorden1 (N x i d) = postorden1 i ++ postorden1 d ++ [x]

-- 2ª definición de postorden
-- ==========================

postorden2 :: Arbol a -> [a]
postorden2 b = reverse (aux [] b)
  where aux xs (H x)     = x : xs
        aux xs (N x i d) = x : aux (aux xs i) d

-- Comprobación de equivalencia de postorden
-- =========================================

-- La propiedad es
prop_postorden :: Arbol Int -> Bool
prop_postorden x =
  postorden1 x == postorden2 x

-- La comprobación es
--    λ> quickCheck prop_postorden
--    OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (postorden1 (arbol 22))
--    8388607
--    (4.60 secs, 11,006,448,376 bytes)
--    λ> length (postorden2 (arbol 22))
--    8388607
--    (2.70 secs, 1,409,880,808 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_longitud_recorrido :: Arbol Int -> Bool
prop_longitud_recorrido x =
   length (preorden1 x)  == n &&
   length (postorden1 x) == n
   where n = nNodos x + nHojas x

-- (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--      nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
nNodos :: Arbol a -> Int
nNodos (H _)     = 0
nNodos (N _ i d) = 1 + nNodos i + nNodos d

-- (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N _ i d) = nHojas i + nHojas d

-- La comprobación es
--    λ> quickCheck prop_longitud_recorrido
--    OK, passed 100 tests.
