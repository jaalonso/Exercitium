-- Pol_Crea_termino.hs
-- Construcción de términos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    creaTermino :: (Num a, Eq a) => Int -> a -> Polinomio a
-- tal que (creaTermino n a) es el término a*x^n. Por ejemplo,
--    creaTermino 2 5  ==  5*x^2
-- ---------------------------------------------------------------------

module Pol_Crea_termino where

import TAD.Polinomio (Polinomio, polCero, consPol)

creaTermino :: (Num a, Eq a) => Int -> a -> Polinomio a
creaTermino n a = consPol n a polCero
