-- Altura_de_un_arbol_binario.hs
-- Altura de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-diciembre-2022
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
--
-- Definir la función
--    altura :: Arbol a -> Int
-- tal que (altura t) es la altura del árbol t. Por ejemplo,
--    λ> altura (Hoja 1)
--    0
--    λ> altura (Nodo (Hoja 1) (Hoja 6))
--    1
--    λ> altura (Nodo (Nodo (Hoja 1) (Hoja 6)) (Hoja 2))
--    2
--    λ> altura (Nodo (Nodo (Hoja 1) (Hoja 6)) (Nodo (Hoja 2) (Hoja 7)))
--    2
-- ---------------------------------------------------------------------

module Altura_de_un_arbol_binario where

data Arbol a = Hoja a
             | Nodo (Arbol a) (Arbol a)

altura :: Arbol a -> Int
altura (Hoja _)   = 0
altura (Nodo i d) = 1 + max (altura i) (altura d)
