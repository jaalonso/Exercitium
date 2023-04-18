-- Polinomios_Transformaciones_polinomios_densas.hs
-- TAD de los polinomios: Transformaciones entre polinomios y listas densas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los polinomios]
-- (https://bit.ly/3KwqXYu) definir las funciones
--    densaApolinomio :: (Num a, Eq a) => [a] -> Polinomio a
--    polinomioAdensa :: (Num a, Eq a) => Polinomio a -> [a]
-- tales que
-- + (densaApolinomio xs) es el polinomio cuya representación densa es
--   xs. Por ejemplo,
--      λ> densaApolinomio [9,0,0,5,0,4,7]
--      9*x^6 + 5*x^3 + 4*x + 7
-- + (polinomioAdensa c) es la representación densa del polinomio p. Por
--   ejemplo,
--      λ> ejPol = consPol 6 9 (consPol 3 5 (consPol 1 4 (consPol 0 7 polCero)))
--      λ> ejPol
--      9*x^6 + 5*x^3 + 4*x + 7
--      λ> polinomioAdensa ejPol
--      [9,0,0,5,0,4,7]
--
-- Comprobar con QuickCheck que ambas funciones son inversas.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Polinomios_Transformaciones_polinomios_densas where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)
import Polinomios_Transformaciones_dispersa_y_densa (densaAdispersa,
                                                     dispersaAdensa)
import Polinomios_Transformaciones_polinomios_dispersas (dispersaApolinomio,
                                                         polinomioAdispersa)
import Polinomios_Coeficiente (coeficiente)
import Data.List (sort, nub)
import Test.QuickCheck

-- 1ª definición de densaApolinomio
-- ================================

densaApolinomio :: (Num a, Eq a) => [a] -> Polinomio a
densaApolinomio []     = polCero
densaApolinomio (x:xs) = consPol (length xs) x (densaApolinomio xs)

-- 2ª definición de densaApolinomio
-- ================================

densaApolinomio2 :: (Num a, Eq a) => [a] -> Polinomio a
densaApolinomio2 = dispersaApolinomio . densaAdispersa

-- La función densaAdispersa está definida en el ejercicio
-- "Transformaciones entre las representaciones dispersa y densa" que se
-- encuentra en https://bit.ly/3GTyIqe

-- La función dispersaApolinomio se encuentra en el ejercicio
-- "Transformaciones entre polinomios y listas dispersas" que se
-- encuentra en https://bit.ly/41GgQaB

-- Comprobación de equivalencia de densaApolinomio
-- ===============================================

-- La propiedad es
prop_densaApolinomio :: [Int] -> Bool
prop_densaApolinomio xs =
  densaApolinomio xs == densaApolinomio2 xs

-- La comprobación es
--    λ> quickCheck prop_densaApolinomio
--    +++ OK, passed 100 tests.

-- 1ª definición de polinomioAdensa
-- ================================

polinomioAdensa :: (Num a, Eq a) => Polinomio a -> [a]
polinomioAdensa p
  | esPolCero p = []
  | otherwise   = [coeficiente k p | k <- [n,n-1..0]]
  where n = grado p

-- La función coeficiente está definida en el ejercicio
-- "Coeficiente del término de grado k" que se encuentra en
-- https://bit.ly/413l3oQ

-- 2ª definición de polinomioAdensa
-- ================================

polinomioAdensa2 :: (Num a, Eq a) => Polinomio a -> [a]
polinomioAdensa2 = dispersaAdensa . polinomioAdispersa

-- Comprobación de equivalencia de polinomioAdensa
-- ===============================================

-- La propiedad es
prop_polinomioAdensa :: Polinomio Int -> Bool
prop_polinomioAdensa p =
  polinomioAdensa p == polinomioAdensa2 p

-- La comprobación es
--    λ> quickCheck prop_polinomioAdensa
--    +++ OK, passed 100 tests.

-- Propiedades de inversa
-- ======================

-- La primera propiedad es
prop_polinomioAdensa_densaApolinomio :: [Int] -> Bool
prop_polinomioAdensa_densaApolinomio xs =
  polinomioAdensa (densaApolinomio xs') == xs'
  where xs' = dropWhile (== 0) xs

-- La comprobación es
--    λ> quickCheck prop_polinomioAdensa_densaApolinomio
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_densaApolinomio_polinomioAdensa :: Polinomio Int -> Bool
prop_densaApolinomio_polinomioAdensa p =
   densaApolinomio (polinomioAdensa p) == p

-- La comprobación es
--    λ> quickCheck prop_densaApolinomio_polinomioAdensa
--    +++ OK, passed 100 tests.
