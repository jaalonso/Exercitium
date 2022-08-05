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

module Media_aritmetica_de_tres_numeros where

media3 :: Float -> Float -> Float -> Float
media3 x y z = (x+y+z)/3
