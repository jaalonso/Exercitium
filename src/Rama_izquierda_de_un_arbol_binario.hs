-- Rama_izquierda_de_un_arbol_binario.hs
-- Rama izquierda de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles binarios con valores en los nodos se pueden definir por
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
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
-- Definir la función
--    ramaIzquierda :: Arbol a -> [a]
-- tal que (ramaIzquierda a) es la lista de los valores de los nodos de
-- la rama izquierda del árbol a. Por ejemplo,
--    λ> ramaIzquierda (N 2 (N 5 (N 3 H H) (N 7 H H)) (N 4 H H))
--    [2,5,3]
-- ---------------------------------------------------------------------

module Rama_izquierda_de_un_arbol_binario where

data Arbol a = H
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

ramaIzquierda :: Arbol a -> [a]
ramaIzquierda H         = []
ramaIzquierda (N x i _) = x : ramaIzquierda i
