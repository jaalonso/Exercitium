-- Arboles_con_bordes_iguales.hs
-- Árboles con bordes iguales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles binarios con valores en las hojas se pueden definir por
--    data Arbol a = H a
--                  | N (Arbol a) (Arbol a)
--                  deriving Show
-- Por ejemplo, los árboles
--    árbol1          árbol2       árbol3     árbol4
--       o              o           o           o
--      / \            / \         / \         / \
--     1   o          o   3       o   3       o   1
--        / \        / \         / \         / \
--       2   3      1   2       1   4       2   3
-- se representan por
--    arbol1, arbol2, arbol3, arbol4 :: Arbol Int
--    arbol1 = N (H 1) (N (H 2) (H 3))
--    arbol2 = N (N (H 1) (H 2)) (H 3)
--    arbol3 = N (N (H 1) (H 4)) (H 3)
--    arbol4 = N (N (H 2) (H 3)) (H 1)
--
-- Definir la función
--    igualBorde :: Eq a => Arbol a -> Arbol a -> Bool
-- tal que (igualBorde t1 t2) se verifica si los bordes de los árboles
-- t1 y t2 son iguales. Por ejemplo,
--    igualBorde arbol1 arbol2  ==  True
--    igualBorde arbol1 arbol3  ==  False
--    igualBorde arbol1 arbol4  ==  False
-- ---------------------------------------------------------------------

module Arboles_con_bordes_iguales where

data Arbol a = N (Arbol a) (Arbol a)
              | H a
              deriving Show

arbol1, arbol2, arbol3, arbol4 :: Arbol Int
arbol1 = N (H 1) (N (H 2) (H 3))
arbol2 = N (N (H 1) (H 2)) (H 3)
arbol3 = N (N (H 1) (H 4)) (H 3)
arbol4 = N (N (H 2) (H 3)) (H 1)

igualBorde :: Eq a => Arbol a -> Arbol a -> Bool
igualBorde t1 t2 = borde t1 == borde t2

-- (borde t) es el borde del árbol t; es decir, la lista de las hojas
-- del árbol t leídas de izquierda a derecha. Por ejemplo,
--    borde arbol4  ==  [2,3,1]
borde :: Arbol a -> [a]
borde (N i d) = borde i ++ borde d
borde (H x)   = [x]
