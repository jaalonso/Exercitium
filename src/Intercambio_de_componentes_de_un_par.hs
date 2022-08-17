-- Intercambio_de_componentes_de_un_par.hs
-- Intercambio de componentes de un par.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p)  es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo,
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
--
-- Comprobar con QuickCheck que la función intercambia es
-- idempotente; es decir, si se aplica dos veces es lo mismo que no
-- aplicarla ninguna.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Intercambio_de_componentes_de_un_par where

import Test.QuickCheck

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

-- La propiedad es
prop_intercambia :: (Int,Int) -> Bool
prop_intercambia p = intercambia (intercambia p) == p

-- La comprobación es
--    λ> quickCheck prop_intercambia
--    +++ OK, passed 100 tests.
