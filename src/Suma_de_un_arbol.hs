-- Suma_de_un_arbol.hs
-- Suma de un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con valores en los nodos]
-- (https://bit.ly/40Pplzj), definir la función
--    sumaArbol :: Num a => Arbol a -> a
-- tal (sumaArbol x) es la suma de los valores que hay en el árbol
-- x. Por ejemplo,
--    sumaArbol (N 2 (N 5 (N 3 H H) (N 7 H H)) (N 4 H H)) == 21
-- ---------------------------------------------------------------------

module Suma_de_un_arbol where

import Arbol_binario_valores_en_nodos (Arbol (H, N))

sumaArbol :: Num a => Arbol a -> a
sumaArbol H         = 0
sumaArbol (N x i d) = x + sumaArbol i + sumaArbol d
