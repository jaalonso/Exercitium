-- Pol_Resta_de_polinomios.hs
-- TAD de los polinomios: Resta de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    restaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (restaPol p q) es el polinomio obtenido restándole a p el
-- q. Por ejemplo,
--    λ> ejPol1 = consPol 5 1 (consPol 4 5 (consPol 2 5 (consPol 0 9 polCero)))
--    λ> ejPol2 = consPol 4 3 (consPol 2 5 (consPol 0 3 polCero))
--    λ> ejPol1
--    x^5 + 5*x^4 + 5*x^2 + 9
--    λ> ejPol2
--    3*x^4 + 5*x^2 + 3
--    λ> restaPol ejPol1 ejPol2
--    x^5 + 2*x^4 + 6
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Resta_de_polinomios where

import TAD.Polinomio (Polinomio, polCero, consPol)
import Pol_Suma_de_polinomios (sumaPol)
import Pol_Crea_termino (creaTermino)
import Pol_Producto_polinomios (multPorTerm)

restaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
restaPol p q  =
  sumaPol p (multPorTerm (creaTermino 0 (-1)) q)
