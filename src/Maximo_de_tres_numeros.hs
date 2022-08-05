-- Maximo_de_tres_numeros.hs
-- Máximo de tres números.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    maxTres :: Int -> Int -> Int -> Int
-- tal que (maxTres x y z) es el máximo de x, y y z. Por ejemplo,
--    maxTres 6 2 4  ==  6
--    maxTres 6 7 4  ==  7
--    maxTres 6 7 9  ==  9
-- ---------------------------------------------------------------------

maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)
