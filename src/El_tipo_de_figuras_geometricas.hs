-- El_tipo_de_figuras_geometricas.hs
-- El tipo de figuras geométricas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran las figuras geométricas formadas por circulos
-- (definidos por su radio) y rectángulos (definidos por su base y su
-- altura). El tipo de las figura geométricas se define por
--    data Figura = Circulo Float | Rect Float Float
--
-- Definir las funciones
--    area     :: Figura -> Float
--    cuadrado :: Float -> Figura
-- tales que
-- + (area f) es el área de la figura f. Por ejemplo,
--      area (Circulo 1)   ==  3.1415927
--      area (Circulo 2)   ==  12.566371
--      area (Rect 2 5)    ==  10.0
-- + (cuadrado n) es el cuadrado de lado n. Por ejemplo,
--      area (cuadrado 3)  ==  9.0
-- ---------------------------------------------------------------------

module El_tipo_de_figuras_geometricas where

data Figura = Circulo Float | Rect Float Float

area :: Figura -> Float
area (Circulo r) = pi*r^2
area (Rect x y)  = x*y

cuadrado :: Float -> Figura
cuadrado n = Rect n n
