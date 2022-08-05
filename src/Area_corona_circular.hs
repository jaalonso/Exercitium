-- Area_corona_circular.hs
-- Área de la corona circular.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    areaDeCoronaCircular :: Double -> Double -> Double
-- tal que (areaDeCoronaCircular r1 r2) es el área de una corona
-- circular de radio interior r1 y radio exterior r2. Por ejemplo,
--    areaDeCoronaCircular 1 2 == 9.42477796076938
--    areaDeCoronaCircular 2 5 == 65.97344572538566
--    areaDeCoronaCircular 3 5 == 50.26548245743669
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Area_corona_circular where

areaDeCoronaCircular :: Double -> Double -> Double
areaDeCoronaCircular r1 r2 = pi*(r2^2 -r1^2)
