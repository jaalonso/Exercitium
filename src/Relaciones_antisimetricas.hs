-- Relaciones_antisimetricas.hs
-- Relaciones antisimétricas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    antisimetrica :: Eq a => Rel a -> Bool
-- tal que (antisimetrica r) se verifica si la relación r es
-- antisimétrica; es decir, si (x,y) e (y,x) están relacionado, entonces
-- x=y. Por ejemplo,
--    antisimetrica (R ([1,2],[(1,2)]))        ==  True
--    antisimetrica (R ([1,2],[(1,2),(2,1)]))  ==  False
--    antisimetrica (R ([1,2],[(1,1),(2,1)]))  ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_antisimetricas where

import Relaciones_binarias (Rel(R))
import Test.QuickCheck

-- 1ª solución
-- ===========

antisimetrica :: Eq a => Rel a -> Bool
antisimetrica (R (_,g)) =
  null [(x,y) | (x,y) <- g, x /= y, (y,x) `elem` g]

-- 2ª solución
-- ===========

antisimetrica2 :: Eq a => Rel a -> Bool
antisimetrica2 (R (_,g)) =
  and [(y,x) `notElem` g | (x,y) <- g, x /= y]


-- 3ª solución
-- ===========

antisimetrica3 :: Eq a => Rel a -> Bool
antisimetrica3 (R (_,g)) =
  all (\(x, y) -> (y,x) `notElem` g || x == y) g


-- 4ª solución
-- ===========

antisimetrica4 :: Eq a => Rel a -> Bool
antisimetrica4 (R (u,g)) =
  and [((x,y) `elem` g && (y,x) `elem` g) --> (x == y)
       | x <- u, y <- u]
  where p --> q = not p || q

-- 5ª solución
-- ===========

antisimetrica5 :: Eq a => Rel a -> Bool
antisimetrica5 (R (_,g)) = aux g
  where aux []         = True
        aux ((x,y):g') = ((y,x) `notElem` g || x == y) && aux g'

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_antisimetrica :: Rel Int -> Bool
prop_antisimetrica r =
  all (== antisimetrica r)
      [antisimetrica2 r,
       antisimetrica3 r,
       antisimetrica4 r,
       antisimetrica5 r]

-- La comprobación es
--    λ> quickCheck prop_antisimetrica
--    +++ OK, passed 100 tests.
