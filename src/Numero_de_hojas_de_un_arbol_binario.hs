-- Numero_de_hojas_de_un_arbol_binario.hs
-- Número de hojas y de nodos de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir las funciones
--    nHojas :: Arbol a -> Int
--    nNodos :: Arbol a -> Int
-- tales que
-- + (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--      nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- + (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--      nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
--
-- Comprobar con QuickCheck que en todo árbol binario el número de sus
-- hojas es igual al número de sus nodos más uno.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numero_de_hojas_de_un_arbol_binario where

import Arboles_binarios (Arbol (..))
import Test.QuickCheck

nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N _ i d) = nHojas i + nHojas d

nNodos :: Arbol a -> Int
nNodos (H _)     = 0
nNodos (N _ i d) = 1 + nNodos i + nNodos d

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_nHojas :: Arbol Int -> Bool
prop_nHojas x =
  nHojas x == nNodos x + 1

-- La comprobación es
--    λ> quickCheck prop_nHojas
--    OK, passed 100 tests.
