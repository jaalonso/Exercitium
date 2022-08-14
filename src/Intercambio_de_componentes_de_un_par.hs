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
-- ---------------------------------------------------------------------

module Intercambio_de_componentes_de_un_par where

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)
