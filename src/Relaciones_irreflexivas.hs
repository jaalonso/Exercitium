-- Relaciones_irreflexivas.hs
-- Relaciones irreflexivas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 07-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    irreflexiva :: Eq a => Rel a -> Bool
-- tal que (irreflexiva r) se verifica si la relación r es irreflexiva;
-- es decir, si ningún elemento de su universo está relacionado con
-- él mismo. Por ejemplo,
--    irreflexiva (R ([1,2,3],[(1,2),(2,1),(2,3)]))  ==  True
--    irreflexiva (R ([1,2,3],[(1,2),(2,1),(3,3)]))  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_irreflexivas where

import Relaciones_binarias (Rel(R))
import Test.QuickCheck

-- 1ª solución
-- ===========

irreflexiva :: Eq a => Rel a -> Bool
irreflexiva (R (u,g)) = and [(x,x) `notElem` g | x <- u]

-- 2ª solución
-- ===========

irreflexiva2 :: Eq a => Rel a -> Bool
irreflexiva2 (R(u,g)) = all (\x -> (x,x) `notElem` g) u

-- 3ª solución
-- ===========

irreflexiva3 :: Eq a => Rel a -> Bool
irreflexiva3 (R(u,g)) = aux u
  where aux []     = True
        aux (x:xs) = (x,x) `notElem` g && aux xs


-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_irreflexiva :: Rel Int -> Bool
prop_irreflexiva r =
  all (== irreflexiva r)
      [irreflexiva2 r,
       irreflexiva3 r]

-- La comprobación es
--    λ> quickCheck prop_irreflexiva
--    +++ OK, passed 100 tests.
