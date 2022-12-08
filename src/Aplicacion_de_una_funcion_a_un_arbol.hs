-- Aplicacion_de_una_funcion_a_un_arbol.hs
-- Aplicación de una función a un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El árbol binario
--         ·
--        / \
--       /   \
--      ·     ·
--     / \   / \
--    1   4 6   9
-- se puede representar por
--    ejArbol = Nodo (Nodo (Hoja 1) (Hoja 4))
--                   (Nodo (Hoja 6) (Hoja 9))
--
-- El tipo de los árboles binarios se puede definir por
--    data Arbol a = Hoja a
--                 | Nodo (Arbol a) (Arbol a)
--      deriving (Show, Eq)
--
-- Definir la función
--    mapArbol :: (a -> b) -> Arbol a -> Arbol b
-- tal que (mapArbol f t) es el árbolo obtenido aplicando la función f a
-- los elementos del árbol t. Por ejemplo,
--    λ> mapArbol (+ 1) (Nodo (Hoja 2) (Hoja 4))
--    Nodo (Hoja 3) (Hoja 5)
-- ---------------------------------------------------------------------

module Aplicacion_de_una_funcion_a_un_arbol where

data Arbol a = Hoja a
             | Nodo (Arbol a) (Arbol a)
  deriving (Show, Eq)

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f (Hoja a)   = Hoja (f a)
mapArbol f (Nodo l r) = Nodo (mapArbol f l) (mapArbol f r)
