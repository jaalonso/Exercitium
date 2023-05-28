-- Grafo_Lema_del_apreton_de_manos.hs
-- TAD de los grafos: Lema del apretón de manos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En la teoría de grafos, se conoce como "Lema del apretón de manos" la
-- siguiente propiedad: la suma de los grados de los vértices de g es el
-- doble del número de aristas de g.
--
-- Comprobar con QuickCheck que para cualquier grafo g, se verifica
-- dicha propiedad.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Grafo_Lema_del_apreton_de_manos where

import TAD.Grafo (Grafo, nodos)
import TAD.GrafoGenerador
import Grafo_Grado_de_un_vertice (grado)
import Grafo_Numero_de_aristas_de_un_grafo (nAristas)
import Test.QuickCheck

prop_apretonManos :: Grafo Int Int -> Bool
prop_apretonManos g =
  sum [grado g v | v <- nodos g] == 2 * nAristas g

-- La comprobación es
--    λ> quickCheck prop_apretonManos
--    +++ OK, passed 100 tests.
