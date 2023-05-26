-- Grafo_Propiedades_de_grados_positivos_y_negativos.hs
-- TAD de los grafos: Propiedades de grados positivos y negativos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Comprobar con QuickCheck que para cualquier grafo g, las
-- sumas de los grados positivos y la de los grados negativos de los
-- vértices de g son iguales
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Grafo_Propiedades_de_grados_positivos_y_negativos where

import TAD.Grafo (Grafo, nodos)
import TAD.GrafoGenerador
import Grafo_Grados_positivos_y_negativos (gradoPos, gradoNeg)
import Test.QuickCheck

-- La propiedad es
prop_sumaGrados :: Grafo Int Int -> Bool
prop_sumaGrados g =
  sum [gradoPos g v | v <- vs] == sum [gradoNeg g v | v <- vs]
  where vs = nodos g

-- La comprobación es
--    λ> quickCheck prop_sumaGrados
--    +++ OK, passed 100 tests.
