-- Media_aritmetica_de_tres_numeros.hs
-- Media aritmética de tres números.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    media3 :: Float -> Float -> Float -> Float
-- tal que (media3 x y z) es la media aritmética de los números x, y y
-- z. Por ejemplo,
--    media3 1 3 8     ==  4.0
--    media3 (-1) 0 7  ==  2.0
--    media3 (-3) 0 3  ==  0.0
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Media_aritmetica_de_tres_numeros where

import Test.QuickCheck

-- 1ª solución
-- ===========

media3a :: Float -> Float -> Float -> Float
media3a x y z = (x+y+z)/3

-- 2ª solución
-- ===========

media3b :: Float -> Float -> Float -> Float
media3b x y z = sum [x, y, z] / 3

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_media3 :: Float -> Float -> Float -> Bool
prop_media3 x y z =
  media3a x y z == media3b x y z

-- La comprobación es
--    λ> quickCheck prop_media3
--    +++ OK, passed 100 tests.
