-- Relaciones_reflexivas.hs
-- Relaciones reflexivas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    reflexiva :: Eq a => Rel a -> Bool
-- tal que (reflexiva r) se verifica si la relación r es reflexiva. Por
-- ejemplo,
--    reflexiva (R ([1,3],[(1,1),(1,3),(3,3)]))    ==  True
--    reflexiva (R ([1,2,3],[(1,1),(1,3),(3,3)]))  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_reflexivas where

import Relaciones_binarias (Rel(R))
import Test.QuickCheck

-- 1ª solución
-- ===========

reflexiva :: Eq a => Rel a -> Bool
reflexiva (R ([], _))   = True
reflexiva (R (x:xs, ps)) = (x, x) `elem` ps && reflexiva  (R (xs, ps))

-- 2ª solución
-- ===========

reflexiva2 :: Eq a => Rel a -> Bool
reflexiva2 (R (us,ps)) = and [(x,x) `elem` ps | x <- us]

-- 3ª solución
-- ===========

reflexiva3 :: Eq a => Rel a -> Bool
reflexiva3 (R (us,ps)) = all (`elem` ps) [(x,x) | x <- us]

-- 4ª solución
-- ===========

reflexiva4 :: Eq a => Rel a -> Bool
reflexiva4 (R (us,ps)) = all (\x -> (x,x) `elem` ps) us

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_reflexiva :: Rel Int -> Bool
prop_reflexiva r =
  all (== reflexiva r)
      [reflexiva2 r,
       reflexiva3 r,
       reflexiva4 r]

-- La comprobación es
--    λ> quickCheck prop_reflexiva
--    +++ OK, passed 100 tests.
