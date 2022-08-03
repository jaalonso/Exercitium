-- Suma_de_monedas.hs
-- Suma de monedas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
-- tal que (sumaMonedas a b c d e) es la suma de los euros
-- correspondientes a a monedas de 1 euro, b de 2 euros, c de 5 euros, d
-- 10 euros y e de 20 euros. Por ejemplo,
--    sumaMonedas 0 0 0 0 1  ==  20
--    sumaMonedas 0 0 8 0 3  == 100
--    sumaMonedas 1 1 1 1 1  ==  38
-- ---------------------------------------------------------------------

module Suma_de_monedas where

sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = 1*a+2*b+5*c+10*d+20*e
