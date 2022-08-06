-- Los_primeros_al_final.hs
-- Los primeros al final
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    rota :: Int -> [a] -> [a]
-- tal que (rota n xs) es la lista obtenida poniendo los n primeros
-- elementos de xs al final de la lista. Por ejemplo,
--    rota 1 [3,2,5,7]  ==  [2,5,7,3]
--    rota 2 [3,2,5,7]  ==  [5,7,3,2]
--    rota 3 [3,2,5,7]  ==  [7,3,2,5]
-- ---------------------------------------------------------------------

module Los_primeros_al_final where

rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs
