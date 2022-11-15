-- Elementos_consecutivos_relacionados.hs
-- Elementos consecutivos relacionados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    relacionados :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionados r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionados (<) [2,3,7,9]                ==  True
--    relacionados (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

module Elementos_consecutivos_relacionados where

-- 1ª solución
-- ===========

relacionados1 :: (a -> a -> Bool) -> [a] -> Bool
relacionados1 r xs = and [r x y | (x,y) <- zip xs (tail xs)]

-- 2ª solución
-- ===========

relacionados2 :: (a -> a -> Bool) -> [a] -> Bool
relacionados2 r (x:y:zs) = r x y && relacionados2 r (y:zs)
relacionados2 _ _        = True

-- 3ª solución
-- ===========

relacionados3 :: (a -> a -> Bool) -> [a] -> Bool
relacionados3 r xs = and (zipWith r xs (tail xs))

-- 4ª solución
-- ===========

relacionados4 :: (a -> a -> Bool) -> [a] -> Bool
relacionados4 r xs = all (uncurry r) (zip xs (tail xs))
