-- Arbol_con_las_hojas_en_la_profundidad_dada.hs
-- Árbol con las hojas en la profundidad dada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-diciembre-2022
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
--    creaArbol :: Int -> Arbol ()
-- tal que (creaArbol n) es el árbol cuyas hoyas están en la profundidad
-- n. Por ejemplo,
--    λ> creaArbol 2
--    Nodo (Nodo (Hoja ()) (Hoja ())) (Nodo (Hoja ()) (Hoja ()))
-- ---------------------------------------------------------------------

module Arbol_con_las_hojas_en_la_profundidad_dada where

data Arbol a = Hoja a
             | Nodo (Arbol a) (Arbol a)
  deriving (Show, Eq)

creaArbol :: Int -> Arbol ()
creaArbol h
  | h <= 0    = Hoja ()
  | otherwise = Nodo x x
  where x = creaArbol (h - 1)
