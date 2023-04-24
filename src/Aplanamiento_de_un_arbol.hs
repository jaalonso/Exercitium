-- Aplanamiento_de_un_arbol.hs
-- Aplanamiento de un árbol
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir la función
--    aplana :: Arbol t -> [t]
-- tal que (aplana a) es la lista obtenida aplanando el árbol a. Por
-- ejemplo,
--    λ> aplana (N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9)))
--    [1,3,4,5,6,7,9]
-- ---------------------------------------------------------------------

module Aplanamiento_de_un_arbol where

import Arboles_binarios (Arbol (..))

aplana :: Arbol t -> [t]
aplana (H n)     = [n]
aplana (N n i d) = aplana i ++ [n] ++ aplana d
