-- Arbol_con_las_hojas_en_la_profundidad_dada.hs
-- Árbol con las hojas en la profundidad dada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con los valores en las hojas]
-- (https://bit.ly/3N5RuyE), definir la función
--    creaArbol :: Int -> Arbol ()
-- tal que (creaArbol n) es el árbol cuyas hoyas están en la profundidad
-- n. Por ejemplo,
--    λ> creaArbol 2
--    Nodo (Nodo (Hoja ()) (Hoja ())) (Nodo (Hoja ()) (Hoja ()))
-- ---------------------------------------------------------------------

module Arbol_con_las_hojas_en_la_profundidad_dada where

import Arbol_binario_valores_en_hojas (Arbol (..))

creaArbol :: Int -> Arbol ()
creaArbol h
  | h <= 0    = Hoja ()
  | otherwise = Nodo x x
  where x = creaArbol (h - 1)
