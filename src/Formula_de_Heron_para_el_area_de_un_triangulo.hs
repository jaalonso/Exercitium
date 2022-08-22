-- Formula_de_Heron_para_el_area_de_un_triangulo.hs
-- Fórmula de Herón para el área de un triángulo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La fórmula de Herón, descubierta por Herón de Alejandría, dice que el
-- área de un triángulo cuyo lados miden a, b y c es la raíz cuadrada de
-- s(s-a)(s-b)(s-c) donde s es el semiperímetro
--    s = (a+b+c)/2
--
-- Definir la función
--    area :: Double -> Double -> Double -> Double
-- tal que (area a b c) es el área del triángulo de lados a, b y c. Por
-- ejemplo,
--    area 3 4 5  ==  6.0
-- ---------------------------------------------------------------------

module Formula_de_Heron_para_el_area_de_un_triangulo where

area :: Double -> Double -> Double -> Double
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2
