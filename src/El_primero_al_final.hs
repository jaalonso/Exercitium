-- El_primero_al_final.hs
-- El primero al final.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    rota1 :: [a] -> [a]
-- tal que (rota1 xs) es la lista obtenida poniendo el primer elemento
-- de xs al final de la lista. Por ejemplo,
--    rota1 [3,2,5,7]  ==  [2,5,7,3]
-- ---------------------------------------------------------------------

module El_primero_al_final where

-- 1ª solución
-- ===========

rota1a :: [a] -> [a]
rota1a xs = tail xs ++ [head xs]

-- 2ª solución
-- ===========

rota1b :: [a] -> [a]
rota1b []     = []
rota1b (x:xs) = xs ++ [x]
