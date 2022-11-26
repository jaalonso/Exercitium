-- El_tipo_de_los_arboles_binarios.hs
-- El tipo de_los árboles binarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El árbol binario
--         5
--        / \
--       /   \
--      3     7
--     / \   / \
--    1   4 6   9
-- se puede representar por
--    ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4))
--                   5
--                   (Nodo (Hoja 6) 7 (Hoja 9))
--
-- El tipo de los árboles binarios se puede definir por
--    data Arbol = Hoja Int
--               | Nodo Arbol Int Arbol
--
-- Definir las funciones
--    ocurre :: Int -> Arbol -> Bool
--    aplana :: Arbol -> [Int]
-- tales que
-- + (ocurre m a) se verifica si m ocurre en el árbol a. Por ejemplo,
--      ocurre 4  ejArbol  ==  True
--      ocurre 10 ejArbol  ==  False
-- + (aplana a) es la lista obtenida aplanando el árbol a. Por ejemplo,
--      aplana ejArbol  ==  [1,3,4,5,6,7,9]
-- ---------------------------------------------------------------------

module El_tipo_de_los_arboles_binarios where

data Arbol = Hoja Int
           | Nodo Arbol Int Arbol

ejArbol :: Arbol
ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4))
               5
               (Nodo (Hoja 6) 7 (Hoja 9))

ocurre :: Int -> Arbol -> Bool
ocurre m (Hoja n)     = m == n
ocurre m (Nodo i n d) = m == n || ocurre m i || ocurre m d

aplana :: Arbol -> [Int]
aplana (Hoja n)     = [n]
aplana (Nodo i n d) = aplana i ++ [n] ++ aplana d
