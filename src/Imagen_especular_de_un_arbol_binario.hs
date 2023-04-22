-- Imagen_especular_de_un_arbol_binario.hs
-- Imagen especular de un árbol binario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir la función
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del árbol x. Por ejemplo,
--    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 7) (N 3 (H 4) (H 2))
--
-- Comprobar con QuickCheck las siguientes propiedades, para todo árbol
-- x,
--    espejo (espejo x) = x
--    reverse (preorden (espejo x)) = postorden x
--    postorden (espejo x) = reverse (preorden x)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Imagen_especular_de_un_arbol_binario where

import Arboles_binarios (Arbol (..))
import Recorrido_de_arboles_binarios (preorden, postorden)
import Test.QuickCheck

espejo :: Arbol a -> Arbol a
espejo (H x)     = H x
espejo (N x i d) = N x (espejo d) (espejo i)

-- Comprobación de las propiedades
-- ===============================

-- Las propiedades son
prop_espejo :: Arbol Int -> Bool
prop_espejo x =
  espejo (espejo x) == x &&
  reverse (preorden (espejo x)) == postorden x &&
  postorden (espejo x) == reverse (preorden x)

-- La comprobación es
--    λ> quickCheck prop_espejo
--    OK, passed 100 tests.
