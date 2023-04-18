-- Pol_Termino_lider.hs
-- TAD de los polinomios: Término líder de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    termLider :: (Num a, Eq a) => Polinomio a -> Polinomio a
-- tal que (termLider p) es el término líder del polinomio p. Por
-- ejemplo,
--    λ> ejPol = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol
--    x^5 + 5*x^2 + 4*x
--    λ> termLider ejPol
--    x^5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Termino_lider where

import TAD.Polinomio (Polinomio, coefLider, grado, polCero, consPol)
import Pol_Crea_termino (creaTermino)

termLider :: (Num a, Eq a) => Polinomio a -> Polinomio a
termLider p = creaTermino (grado p) (coefLider p)

-- La función creaTermino está definida en el ejercicio
-- "Construcción de términos" que se encuentra en
-- https://bit.ly/3GXteuH
