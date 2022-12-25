-- Suma_de_un_arbol.hs
-- Suma de un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-diciembre-2022
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
-- Definir por recursión la función
--    sumaArbol :: Num a => Arbol1 a -> a
-- tal (sumaArbol x) es la suma de los valores que hay en el árbol
-- x. Por ejemplo,
--    sumaArbol (N 2 (N 5 (N 3 H H) (N 7 H H)) (N 4 H H)) == 21
-- ---------------------------------------------------------------------

module Suma_de_un_arbol where

data Arbol1 a = H
              | N a (Arbol1 a) (Arbol1 a)
  deriving (Show, Eq)

sumaArbol :: Num a => Arbol1 a -> a
sumaArbol H         = 0
sumaArbol (N x i d) = x + sumaArbol i + sumaArbol d
