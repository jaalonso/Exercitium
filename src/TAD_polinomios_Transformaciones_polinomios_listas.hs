-- TAD_polinomios_Transformaciones_polinomios_listas.hs
-- TAD de los polinomios: Transformaciones entre polinomios y listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los polinomios](???)
-- definir las funciones
--    listaApolinomio :: (Num a, Eq a) => [a] -> Polinomio a
--    polinomioAlista :: Conj a -> [a]
-- tales que
-- + (listaApolinomio xs) es el polinomio formado por los elementos de xs.
--   Por ejemplo,
--      λ> listaApolinomio [6,0,-2,4,-7]
--      6*x^4 + -2*x^2 + 4*x + -7
-- + (polinomioAlista c) es la lista formada por los elementos del
--   polinomio c. Por ejemplo,
--
-- Comprobar con QuickCheck que ambas funciones son inversa; es decir,
--    polinomioAlista (listaApolinomio xs) = sort (nub xs)
--    listaApolinomio (polinomioAlista c)  = c
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_polinomios_Transformaciones_polinomios_listas where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)
import Data.List (sort, nub)
import Test.QuickCheck

-- 1ª definición de listaApolinomio
-- ================================

listaApolinomio1 :: (Num a, Eq a) => [a] -> Polinomio a
listaApolinomio1 []     = polCero
listaApolinomio1 (x:xs) = consPol (length xs) x (listaApolinomio1 xs)

-- 2ª definición de listaApolinomio
-- ================================

listaApolinomio2 :: (Num a, Eq a) => [a] -> Polinomio a
listaApolinomio2 = dispersaApolinomio1. densaAdispersa

-- (densaAdispersa xs) es la representación densa del polinomio cuya
-- representación dispersa es xs. Por ejemplo,
--    λ> densaAdispersa [6,0,-2,4,-7]
--    [(4,6),(3,0),(2,-2),(1,4),(0,-7)]
densaAdispersa :: [a] -> [(Int,a)]
densaAdispersa xs = zip [n-1,n-2..] xs
  where n  = length xs

-- (dispersaApolinomio ps) es el polinomio cuya representación densa es
-- ps. Por ejemplo,
--    λ> dispersaApolinomio1 [(4,6),(3,0),(2,-2),(1,4),(0,-7)]
--    6*x^4 + -2*x^2 + 4*x + -7
dispersaApolinomio1 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
dispersaApolinomio1 []         = polCero
dispersaApolinomio1 ((n,a):ps) = consPol n a (dispersaApolinomio1 ps)

-- 2ª definición
dispersaApolinomio2 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
dispersaApolinomio2 = foldr (\(x,y) -> consPol x y) polCero

-- 3ª definición
dispersaApolinomio3 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
dispersaApolinomio3 = foldr (uncurry consPol) polCero


-- -- 2ª definición de listaApolinomio
-- -- ===============================
--
-- listaApolinomio2 :: Ord a => [a] -> Pol a
-- listaApolinomio2 = foldr inserta vacio
--
-- -- Comprobación de equivalencia
-- -- ============================
--
-- -- La propiedad es
-- prop_listaApolinomio :: [Int] -> Bool
-- prop_listaApolinomio xs =
--   listaApolinomio xs == listaApolinomio2 xs
--
-- -- La comprobación es
-- --    λ> quickCheck prop_listaApolinomio
-- --    +++ OK, passed 100 tests.
--
-- -- Definición de polinomioAlista
-- -- ============================
--
-- polinomioAlista :: Ord a => Pol a -> [a]
-- polinomioAlista c
--   | esVacio c = []
--   | otherwise = mc : polinomioAlista rc
--   where mc = menor c
--         rc = elimina mc c
--
-- -- Comprobación de las propiedades
-- -- ===============================
--
-- -- La primera propiedad es
-- prop_1_listaApolinomio :: [Int] -> Bool
-- prop_1_listaApolinomio xs =
--   polinomioAlista (listaApolinomio xs) == sort (nub xs)
--
-- -- La comprobación es
-- --    λ> quickCheck prop_1_listaApolinomio
-- --    +++ OK, passed 100 tests.
--
-- -- La segunda propiedad es
-- prop_2_listaApolinomio :: Pol Int -> Bool
-- prop_2_listaApolinomio c =
--   listaApolinomio (polinomioAlista c) == c
--
-- -- La comprobación es
-- --    λ> quickCheck prop_2_listaApolinomio
-- --    +++ OK, passed 100 tests.
