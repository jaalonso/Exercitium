-- Elementos_del_nivel_k_de_un_arbol.hs
-- Elementos del nivel k de un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles binarios con valores en las hojas y en los nodos se
-- definen por
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
-- Por ejemplo, el árbol
--         7
--        / \
--       /   \
--      2     9
--     / \
--    5   4
-- se representa por
--    N 7 (N 2 (H 5) (H 4)) (H 9)
--
-- Un elemento de un árbol se dirá de nivel k si aparece en el árbol a
-- distancia k  de la raíz.
--
-- Definir la función
--    nivel :: Int -> Arbol a -> [a]
-- tal que (nivel k a) es la lista de los elementos de nivel k del árbol
-- a. Por ejemplo,
--    nivel 0 (H 5)                          ==  [5]
--    nivel 1 (H 5)                          ==  []
--    nivel 0 (N 7 (N 2 (H 5) (H 4)) (H 9))  ==  [7]
--    nivel 1 (N 7 (N 2 (H 5) (H 4)) (H 9))  ==  [2,9]
--    nivel 2 (N 7 (N 2 (H 5) (H 4)) (H 9))  ==  [5,4]
--    nivel 3 (N 7 (N 2 (H 5) (H 4)) (H 9))  ==  []
-- ---------------------------------------------------------------------

module Elementos_del_nivel_k_de_un_arbol where

data Arbol a = H a
             | N a (Arbol a) (Arbol a)

nivel :: Int -> Arbol a -> [a]
nivel 0 (H x)      = [x]
nivel 0 (N x _  _) = [x]
nivel _ (H _ )     = []
nivel k (N _ i d)  = nivel (k-1) i ++ nivel (k-1) d
