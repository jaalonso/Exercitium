-- Distancia_entre_dos_puntos.hs
-- Distancia entre dos puntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo,
--    distancia (1,2) (4,6)  ==  5.0

-- Comprobar con QuickCheck que se verifica la propiedad triangular de
-- la distancia; es decir, dados tres puntos p1, p2 y p3, la distancia
-- de p1 a p3 es menor o igual que la suma de la distancia de p1 a p2 y
-- la de p2 a p3.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Distancia_entre_dos_puntos where

import Test.QuickCheck

distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

-- La propiedad es
prop_triangular :: (Double,Double) -> (Double,Double) -> (Double,Double)
                -> Bool
prop_triangular p1 p2 p3 =
    distancia p1 p3 <= distancia p1 p2 + distancia p2 p3

-- La comprobación es
--    ghci> quickCheck prop_triangular
--    +++ OK, passed 100 tests.
