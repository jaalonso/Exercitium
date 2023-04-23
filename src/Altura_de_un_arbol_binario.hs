-- Altura_de_un_arbol_binario.hs
-- Altura de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con los valores en las hojas]
-- (https://bit.ly/3N5RuyE), definir la función
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

import Arbol_binario_valores_en_hojas (Arbol (..))

altura :: Arbol a -> Int
altura (Hoja _)   = 0
altura (Nodo i d) = 1 + max (altura i) (altura d)
