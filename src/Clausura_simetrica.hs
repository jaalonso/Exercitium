-- Clausura_simetrica.hs
-- Clausura simétrica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    clausuraSimetrica :: Eq a => Rel a -> Rel a
-- tal que (clausuraSimetrica r) es la clausura simétrica de r; es
-- decir, la menor relación simétrica que contiene a r. Por ejemplo,
--    λ> clausuraSimetrica (R ([1,3,5],[(1,1),(3,1),(1,5)]))
--    R ([1,3,5],[(1,1),(3,1),(1,5),(1,3),(5,1)])
--
-- Comprobar con QuickCheck que clausuraSimetrica es simétrica.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Clausura_simetrica where

import Relaciones_binarias (Rel(R))
import Data.List (union)
import Relaciones_simetricas (simetrica)
import Test.QuickCheck

clausuraSimetrica :: Eq a => Rel a -> Rel a
clausuraSimetrica (R (u,g)) =
  R (u, g `union` [(y,x) | (x,y) <- g])

-- La propiedad es
prop_ClausuraSimetrica :: Rel Int -> Bool
prop_ClausuraSimetrica r =
  simetrica (clausuraSimetrica r)

-- La función simetrica está definida en el ejercicio
-- "Relaciones simétricas" que se encuentra en
-- https://bit.ly/3zlO2rH

-- La comprobación es
--    λ> quickCheck prop_ClausuraSimetrica
--    +++ OK, passed 100 tests.
