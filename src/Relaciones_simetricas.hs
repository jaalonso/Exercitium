-- Relaciones_simetricas.hs
-- Relaciones simétricas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    simetrica :: Eq a => Rel a -> Bool
-- tal que (simetrica r) se verifica si la relación r es simétrica. Por
-- ejemplo,
--    simetrica (R ([1,3],[(1,1),(1,3),(3,1)]))  ==  True
--    simetrica (R ([1,3],[(1,1),(1,3),(3,2)]))  ==  False
--    simetrica (R ([1,3],[]))                   ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_simetricas where

import Relaciones_binarias (Rel(R))
import Test.QuickCheck

-- 1ª solución
-- ===========

simetrica :: Eq a => Rel a -> Bool
simetrica (R (_,g)) = and [(y,x) `elem` g | (x,y) <- g]

-- 2ª solución
-- ===========

simetrica2 :: Eq a => Rel a -> Bool
simetrica2 (R (_,g)) = all (\(x,y) -> (y,x) `elem` g) g

-- 3ª solución
-- ===========

simetrica3 :: Eq a => Rel a -> Bool
simetrica3 (R (_,g)) = aux g
  where aux [] = True
        aux ((x,y):ps) = (y,x) `elem` g && aux ps

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_simetrica :: Rel Int -> Bool
prop_simetrica r =
  all (== simetrica r)
      [simetrica2 r,
       simetrica3 r]

-- La comprobación es
--    λ> quickCheck prop_simetrica
--    +++ OK, passed 100 tests.
