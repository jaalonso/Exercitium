-- Profundidad_de_un_arbol_binario.hs
-- Profundidad de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir las funciones
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
--    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
--    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  ==  2
--
-- Comprobar con QuickCheck que para todo árbol biario x, se tiene que
--    nNodos x <= 2^(profundidad x) - 1
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Profundidad_de_un_arbol_binario where
import Arboles_binarios (Arbol (..))
import Numero_de_hojas_de_un_arbol_binario (nNodos)

import Test.QuickCheck

profundidad :: Arbol a -> Int
profundidad (H _)     = 0
profundidad (N _ i d) = 1 + max (profundidad i) (profundidad d)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_nNodosProfundidad :: Arbol Int -> Bool
prop_nNodosProfundidad x =
   nNodos x <= 2 ^ profundidad x - 1

-- La comprobación es
--    λ> quickCheck prop_nNodosProfundidad
--    OK, passed 100 tests.
