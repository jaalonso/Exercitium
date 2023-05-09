-- Pol_Regla_de_Ruffini.hs
-- TAD de los polinomios: Regla de Ruffini
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir las funciones
--    cocienteRuffini :: Int -> Polinomio Int -> Polinomio Int
--    restoRuffini    :: Int -> Polinomio Int -> Int
-- tales que
-- + (cocienteRuffini r p) es el cociente de dividir el polinomio p por
--   el polinomio x-r. Por ejemplo:
--      λ> ejPol = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
--      λ> ejPol
--      x^3 + 2*x^2 + -1*x + -2
--      λ> cocienteRuffini 2 ejPol
--      x^2 + 4*x + 7
--      λ> cocienteRuffini (-2) ejPol
--      x^2 + -1
--      λ> cocienteRuffini 3 ejPol
--      x^2 + 5*x + 14
-- + (restoRuffini r p) es el resto de dividir el polinomio p por el
--   polinomio x-r. Por ejemplo,
--      λ> restoRuffini 2 ejPol
--      12
--      λ> restoRuffini (-2) ejPol
--      0
--      λ> restoRuffini 3 ejPol
--      40
--
-- Comprobar con QuickCheck que, dado un polinomio p y un número entero
-- r, las funciones anteriores verifican la propiedad de la división
-- euclídea.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Regla_de_Ruffini where

import TAD.Polinomio (Polinomio, consPol, polCero)
import Pol_Transformaciones_polinomios_densas (densaApolinomio,
                                               polinomioAdensa)
import Pol_Division_de_Ruffini_con_representacion_densa (ruffiniDensa)
import Pol_Producto_polinomios (multPol)
import Pol_Suma_de_polinomios (sumaPol)
import Pol_Crea_termino (creaTermino)
import Test.QuickCheck

-- 1ª definición de cocienteRuffini
-- ================================

cocienteRuffini :: Int -> Polinomio Int -> Polinomio Int
cocienteRuffini r p = densaApolinomio (init (ruffiniDensa r (polinomioAdensa p)))

-- 2ª definición de cocienteRuffini
-- ================================

cocienteRuffini2 :: Int -> Polinomio Int -> Polinomio Int
cocienteRuffini2 r = densaApolinomio . ruffiniDensa r . init . polinomioAdensa

-- 1ª definición de restoRuffini
-- =============================

restoRuffini :: Int -> Polinomio Int -> Int
restoRuffini r p = last (ruffiniDensa r (polinomioAdensa p))

-- 2ª definición de restoRuffini
-- =============================

restoRuffini2 :: Int -> Polinomio Int -> Int
restoRuffini2 r = last . ruffiniDensa r . polinomioAdensa

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_diviEuclidea :: Int -> Polinomio Int -> Bool
prop_diviEuclidea r p =
  p == sumaPol (multPol coci divi) rest
  where coci = cocienteRuffini r p
        divi = densaApolinomio [1,-r]
        rest = creaTermino 0 (restoRuffini r p)

-- La comprobación es
--    λ> quickCheck prop_diviEuclidea
--    +++ OK, passed 100 tests.
