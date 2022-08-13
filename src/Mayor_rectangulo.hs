-- Mayor_rectangulo.hs
-- Mayor rectángulo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las dimensiones de los rectángulos puede representarse por pares; por
-- ejemplo, (5,3) representa a un rectángulo de base 5 y altura 3.
--
-- Definir la función
--    mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
-- tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre
-- r1 y r2. Por ejemplo,
--    mayorRectangulo (4,6) (3,7)  ==  (4,6)
--    mayorRectangulo (4,6) (3,8)  ==  (4,6)
--    mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ---------------------------------------------------------------------

module Mayor_rectangulo where

mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo (a,b) (c,d)
  | a*b >= c*d = (a,b)
  | otherwise  = (c,d)
