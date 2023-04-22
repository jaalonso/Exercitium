-- Recorrido_de_arboles_binarios.hs
-- Recorrido de árboles binarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir las funciones
--    preorden  :: Arbol a -> [a]
--    postorden :: Arbol a -> [a]
-- tales que
-- + (preorden x) es la lista correspondiente al recorrido preorden del
--   árbol x; es decir, primero visita la raíz del árbol, a continuación
--   recorre el subárbol izquierdo y, finalmente, recorre el subárbol
--   derecho. Por ejemplo,
--      preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- + (postorden x) es la lista correspondiente al recorrido postorden
--   del árbol x; es decir, primero recorre el subárbol izquierdo, a
--   continuación el subárbol derecho y, finalmente, la raíz del
--   árbol. Por ejemplo,
--      postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
--
-- Comprobar con QuickCheck que la longitud de la lista
-- obtenida recorriendo un árbol en cualquiera de los sentidos es igual
-- al número de nodos del árbol más el número de hojas.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Recorrido_de_arboles_binarios where

import Arboles_binarios (Arbol (..))
import Numero_de_hojas_de_un_arbol_binario (nNodos, nHojas)
import Test.QuickCheck

preorden :: Arbol a -> [a]
preorden (H x)     = [x]
preorden (N x i d) = x : preorden i ++ preorden d

postorden :: Arbol a -> [a]
postorden (H x)     = [x]
postorden (N x i d) = postorden i ++ postorden d ++ [x]

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_longitud_recorrido :: Arbol Int -> Bool
prop_longitud_recorrido x =
   length (preorden x)  == n &&
   length (postorden x) == n
   where n = nNodos x + nHojas x

-- La comprobación es
--    λ> quickCheck prop_longitud_recorrido
--    OK, passed 100 tests.
