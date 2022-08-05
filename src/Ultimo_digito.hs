-- Ultimo_digito.hs
-- Último dígito.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ultimoDigito :: Int -> Int
-- tal que (ultimoDigito x) es el último dígito del número x. Por
-- ejemplo,
--    ultimoDigito 325  ==  5
-- ---------------------------------------------------------------------

module Ultimo_digito where

ultimoDigito :: Int -> Int
ultimoDigito x = rem x 10
