-- Pertenencia_de_un_elemento_a_un_arbol.hs
-- Pertenencia de un elemento a un árbol
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir la función
--    pertenece :: Eq t => t -> Arbol t -> Bool
-- tal que (pertenece m a) se verifica si m pertenece en el árbol a. Por
-- ejemplo,
--    λ> pertenece 4 (N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9)))
--    True
--    λ> pertenece 0 (N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9)))
--    False
-- ---------------------------------------------------------------------

module Pertenencia_de_un_elemento_a_un_arbol where

import Arboles_binarios (Arbol (..))

pertenece :: Eq t => t -> Arbol t -> Bool
pertenece m (H n)     = m == n
pertenece m (N n i d) = m == n || pertenece m i || pertenece m d
