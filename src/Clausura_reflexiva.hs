-- Clausura_reflexiva.hs
-- Clausura reflexiva.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    clausuraReflexiva :: Eq a => Rel a -> Rel a
-- tal que (clausuraReflexiva r) es la clausura reflexiva de r; es
-- decir, la menor relación reflexiva que contiene a r. Por ejemplo,
--    λ> clausuraReflexiva (R ([1,3],[(1,1),(3,1)]))
--    R ([1,3],[(1,1),(3,1),(3,3)])
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Clausura_reflexiva where

import Relaciones_binarias (Rel(R))
import Data.List (union)
import Test.QuickCheck (quickCheck)

clausuraReflexiva :: Eq a => Rel a -> Rel a
clausuraReflexiva (R (u,g)) =
  R (u, g `union` [(x,x) | x <- u])
