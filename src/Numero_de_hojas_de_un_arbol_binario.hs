-- Numero_de_hojas_de_un_arbol_binario.hs
-- Número de hojas de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran los árboles binarios definidos como sigue
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
-- Por ejemplo, el árbol
--         9
--        / \
--       /   \
--      3     7
--     / \
--    2   4
-- se representa por
--    N 9 (N 3 (H 2) (H 4)) (H 7)
--
-- Definir la función
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ---------------------------------------------------------------------

module Numero_de_hojas_de_un_arbol_binario where

data Arbol a = H a
             | N a (Arbol a) (Arbol a)

nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N _ i d) = nHojas i + nHojas d
