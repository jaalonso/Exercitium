-- Existencia_de_elemento_del_arbol_con_propiedad.hs
-- Existencia de elementos del árbol que verifican una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con valores en los nodos y
-- en las hojas](https://bit.ly/3H53exA), definir la función
--    algunoArbol :: Arbol t -> (t -> Bool) -> Bool
-- tal que (algunoArbol a p) se verifica si algún elemento del árbol a
-- cumple la propiedad p. Por ejemplo,
--    algunoArbol (N 5 (N 3 (H 1) (H 4)) (H 2)) (>4)  ==  True
--    algunoArbol (N 5 (N 3 (H 1) (H 4)) (H 2)) (>7)  ==  False
-- ---------------------------------------------------------------------

module Existencia_de_elemento_del_arbol_con_propiedad where

import Arboles_binarios (Arbol (H, N))

algunoArbol :: Arbol a -> (a -> Bool) -> Bool
algunoArbol (H x) p     = p x
algunoArbol (N x i d) p = p x || algunoArbol i p || algunoArbol d p
