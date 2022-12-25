-- Arboles_balanceados.hs
-- Árboles balanceados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles binarios con valores en los nodos se pueden definir por
--    data Arbol a = H
--                 | N a (Arbol1 a) (Arbol1 a)
--      deriving (Show, Eq)
-- Por ejemplo, el árbol
--         9
--        / \
--       /   \
--      8     6
--     / \   / \
--    3   2 4   5
-- se puede representar por
--    N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--
-- Diremos que un árbol está balanceado si para cada nodo la diferencia
-- entre el número de nodos de sus subárboles izquierdo y derecho es
-- menor o igual que uno.
--
-- Definir la función
--    balanceado :: Arbol a -> Bool
-- tal que (balanceado a) se verifica si el árbol a está balanceado. Por
-- ejemplo,
--    λ> balanceado (N 5 H (N 3 H H))
--    True
--    λ> balanceado (N 4 (N 3 (N 2 H H) H) (N 5 H (N 6 H (N 7 H H))))
--    False
-- ---------------------------------------------------------------------

module Arboles_balanceados where

data Arbol a = H
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

balanceado :: Arbol a -> Bool
balanceado H         = True
balanceado (N _ i d) = abs (numeroNodos i - numeroNodos d) <= 1
                       && balanceado i
                       && balanceado d

-- (numeroNodos a) es el número de nodos del árbol a. Por ejemplo,
--    numeroNodos (N 5 H (N 3 H H)) ==  2
numeroNodos :: Arbol a -> Int
numeroNodos H         = 0
numeroNodos (N _ i d) = 1 + numeroNodos i + numeroNodos d
