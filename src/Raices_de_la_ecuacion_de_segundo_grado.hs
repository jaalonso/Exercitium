-- Raices_de_la_ecuacion_de_segundo_grado.hs
-- Raíces de la ecuación de segundo grado.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0. Por ejemplo,
--    raices 1 3 2    ==  [-1.0,-2.0]
--    raices 1 (-2) 1 ==  [1.0,1.0]
--    raices 1 0 1    ==  []
--
-- Comprobar con QuickCheck que la suma de las raíces de la ecuación
-- ax^2 + bx + c = 0 (con a no nulo) es -b/a y su  producto es c/a.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Raices_de_la_ecuacion_de_segundo_grado where

import Test.QuickCheck

raices :: Double -> Double -> Double -> [Double]
raices a b c
    | d >= 0    = [(-b+e)/t,(-b-e)/t]
    | otherwise = []
    where d = b^2 - 4*a*c
          e = sqrt d
          t = 2*a

-- Para comprobar la propiedad se usará el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milésima. Por
-- ejemplo,
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
(~=) :: (Fractional a, Ord a) => a -> a -> Bool
x ~= y  = abs (x-y) < 0.001

-- La propiedad es
prop_raices :: Double -> Double -> Double -> Property
prop_raices a b c =
    a /= 0 && not (null xs) ==> sum xs ~= (-b/a) && product xs ~= (c/a)
    where xs = raices a b c

-- La comprobación es
--    λ> quickCheck prop_raices
--    +++ OK, passed 100 tests.
