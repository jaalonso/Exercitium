-- Pol_Producto_polinomios.hs
-- TAD de los polinomios: Producto de polinomios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    multPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (multPol p q) es el producto de los polinomios p y q. Por
-- ejemplo,
--    λ> ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
--    λ> ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol1
--    3*x^4 + -5*x^2 + 3
--    λ> ejPol2
--    x^5 + 5*x^2 + 4*x
--    λ> multPol ejPol1 ejPol2
--    3*x^9 + -5*x^7 + 15*x^6 + 15*x^5 + -25*x^4 + -20*x^3 + 15*x^2 + 12*x
--
-- Comprobar con QuickCheck las siguientes propiedades
-- + El producto de polinomios es conmutativo.
-- + El producto es distributivo respecto de la suma.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Producto_polinomios where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)
import Pol_Termino_lider (termLider)
import Pol_Suma_de_polinomios (sumaPol)
import Test.QuickCheck

multPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
multPol p q
  | esPolCero p = polCero
  | otherwise    = sumaPol (multPorTerm (termLider p) q)
                           (multPol (restoPol p) q)

-- (multPorTerm t p) es el producto del término t por el polinomio
-- p. Por ejemplo,
--    ejTerm                     ==  4*x
--    ejPol2                     ==  x^5 + 5*x^2 + 4*x
--    multPorTerm ejTerm ejPol2  ==  4*x^6 + 20*x^3 + 16*x^2
multPorTerm :: (Num t, Eq t) => Polinomio t -> Polinomio t -> Polinomio t
multPorTerm term pol
  | esPolCero pol = polCero
  | otherwise     = consPol (n+m) (a*b) (multPorTerm term r)
  where n = grado term
        a = coefLider term
        m = grado pol
        b = coefLider pol
        r = restoPol pol

-- El producto de polinomios es conmutativo.
prop_conmutativaProducto :: Polinomio Int -> Polinomio Int -> Bool
prop_conmutativaProducto p q =
  multPol p q == multPol q p

-- La comprobación es
--    λ> quickCheck prop_conmutativaProducto
--    OK, passed 100 tests.

-- El producto es distributivo respecto de la suma.
prop_distributivaProductoSuma :: Polinomio Int -> Polinomio Int
                                 -> Polinomio Int -> Bool
prop_distributivaProductoSuma p q r =
  multPol p (sumaPol q r) == sumaPol (multPol p q) (multPol p r)

-- Comprobación:
--    λ> quickCheck prop_distributivaProductoSuma
--    OK, passed 100 tests.
