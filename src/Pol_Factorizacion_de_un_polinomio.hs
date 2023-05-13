-- Pol_Factorizacion_de_un_polinomio.hs
-- TAD de los polinomios: Factorización de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    factorizacion :: Polinomio Int -> [Polinomio Int]
-- tal que (factorizacion p) es la lista de la descomposición del
-- polinomio p en factores obtenida mediante el regla de Ruffini. Por
-- ejemplo,
--    λ> ejPol1 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol1
--    x^5 + 5*x^2 + 4*x
--    λ> factorizacion ejPol1
--    [1*x,1*x + 1,x^3 + -1*x^2 + 1*x + 4]
--    λ> ejPol2 = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
--    λ> ejPol2
--    x^3 + 2*x^2 + -1*x + -2
--    λ> factorizacion ejPol2
--    [1*x + -1,1*x + 1,1*x + 2,1]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Factorizacion_de_un_polinomio where

import TAD.Polinomio (Polinomio, consPol, polCero, esPolCero)
import Pol_Termino_independiente_de_un_polinomio (terminoIndep)
import Pol_Raices_enteras_de_un_polinomio (divisores)
import Pol_Regla_de_Ruffini (cocienteRuffini)
import Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini (esRaizRuffini)
import Pol_Transformaciones_polinomios_densas (densaApolinomio)

factorizacion :: Polinomio Int -> [Polinomio Int]
factorizacion p
  | esPolCero p = [p]
  | otherwise   = aux (0 : divisores (terminoIndep p))
  where
    aux [] = [p]
    aux (r:rs)
        | esRaizRuffini r p =
            densaApolinomio [1,-r] : factorizacion (cocienteRuffini r p)
        | otherwise = aux rs
