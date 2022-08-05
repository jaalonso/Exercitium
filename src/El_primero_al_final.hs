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

rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]
