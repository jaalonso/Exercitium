-- Rama_izquierda_de_un_arbol_binario.hs
-- Rama izquierda de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con valores en los nodos]
-- (https://bit.ly/40Pplzj), definir la función
--    ramaIzquierda :: Arbol a -> [a]
-- tal que (ramaIzquierda a) es la lista de los valores de los nodos de
-- la rama izquierda del árbol a. Por ejemplo,
--    λ> ramaIzquierda (N 2 (N 5 (N 3 H H) (N 7 H H)) (N 4 H H))
--    [2,5,3]
-- ---------------------------------------------------------------------

module Rama_izquierda_de_un_arbol_binario where

import Arbol_binario_valores_en_nodos (Arbol (H, N))

ramaIzquierda :: Arbol a -> [a]
ramaIzquierda H         = []
ramaIzquierda (N x i _) = x : ramaIzquierda i
