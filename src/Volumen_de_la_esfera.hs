-- Volumen_de_la_esfera.hs
-- Volumen de la esfera.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    volumenEsfera :: Float -> Float
-- tal que (volumenEsfera r) es el volumen de la esfera de radio r. Por
-- ejemplo,
--    volumenEsfera 10  ==  4188.7905
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Volumen_de_la_esfera where

volumenEsfera :: Float -> Float
volumenEsfera r = (4/3)*pi*r^3
