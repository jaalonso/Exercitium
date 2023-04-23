-- Aplicacion_de_una_funcion_a_un_arbol.hs
-- Aplicación de una función a un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con los valores en las hojas]
-- (https://bit.ly/3N5RuyE), definir la función
--    mapArbol :: (a -> b) -> Arbol a -> Arbol b
-- tal que (mapArbol f t) es el árbol obtenido aplicando la función f a
-- los elementos del árbol t. Por ejemplo,
--    λ> mapArbol (+ 1) (Nodo (Hoja 2) (Hoja 4))
--    Nodo (Hoja 3) (Hoja 5)
-- ---------------------------------------------------------------------

module Aplicacion_de_una_funcion_a_un_arbol where

import Arbol_binario_valores_en_hojas (Arbol (..))

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f (Hoja a)   = Hoja (f a)
mapArbol f (Nodo l r) = Nodo (mapArbol f l) (mapArbol f r)
