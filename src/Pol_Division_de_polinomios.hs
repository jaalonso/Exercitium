-- Pol_Division_de_polinomios.hs
-- TAD de los polinomios: División de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir las funciones
--    cociente :: (Fractional a, Eq a) =>
--                Polinomio a -> Polinomio a -> Polinomio a
--    resto    :: (Fractional a, Eq a) =>
--                Polinomio a -> Polinomio a -> Polinomio a
-- tales que
-- + (cociente p q) es el cociente de la división de p entre q. Por
--   ejemplo,
--      λ> pol1 = consPol 3 2 (consPol 2 9 (consPol 1 10 (consPol 0 4 polCero)))
--      λ> pol1
--      2*x^3 + 9*x^2 + 10*x + 4
--      λ> pol2 = consPol 2 1 (consPol 1 3 polCero)
--      λ> pol2
--      x^2 + 3*x
--      λ> cociente pol1 pol2
--      2.0*x + 3.0
-- + (resto p q) es el resto de la división de p entre q. Por ejemplo,
--      λ> resto pol1 pol2
--      1.0*x + 4.0
-- ---------------------------------------------------------------------

module Pol_Division_de_polinomios where

import TAD.Polinomio (Polinomio, polCero, consPol, grado, coefLider)
import Pol_Crea_termino (creaTermino)
import Pol_Producto_polinomios (multPol, multPorTerm)
import Pol_Resta_de_polinomios (restaPol)
import Pol_Multiplicacion_de_un_polinomio_por_un_numero (multEscalar)

cociente :: (Fractional a, Eq a) =>
            Polinomio a -> Polinomio a -> Polinomio a
cociente p q
  | n2 == 0   = multEscalar (1/a2) p
  | n1 < n2   = polCero
  | otherwise = consPol n3 a3 (cociente p3 q)
  where n1 = grado p
        a1 = coefLider p
        n2 = grado q
        a2 = coefLider q
        n3 = n1-n2
        a3 = a1/a2
        p3 = restaPol p (multPorTerm (creaTermino n3 a3) q)

resto :: (Fractional a, Eq a) =>
         Polinomio a -> Polinomio a -> Polinomio a
resto p q = restaPol p (multPol (cociente p q) q)
