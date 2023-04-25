-- Arbol_binario_valores_en_nodos.hs
-- El tipo de los árboles binarios con valores en los nodos
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-diciembre-2022
-- ---------------------------------------------------------------------

-- El árbol, con valores en los nodos,
--            9
--           / \
--          /   \
--         /     \
--        8       6
--       / \     / \
--      3   2   4   5
--     /\  /\  /\   /\
--    ·  ··  ··  · ·  ·
-- se puede representar por
--    N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
-- usando el tipo de los árboles con valores en los nodos definido como
-- se muestra a continuación.

module Arbol_binario_valores_en_nodos where

data Arbol a = H
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)
