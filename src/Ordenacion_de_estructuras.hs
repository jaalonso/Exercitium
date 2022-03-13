-- Ordenacion_de_estructuras.hs
-- Ordenación de estructuras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las notas de los dos primeros exámenes se pueden representar mediante
-- el siguiente tipo de dato
--    data Notas = Notas String Int Int
--      deriving (Read, Show, Eq)
-- Por ejemplo, (Notas "Juan" 6 5) representar las notas de un alumno
-- cuyo nombre es Juan, la nota del primer examen es 6 y la del segundo
-- es 5.
--
-- Definir la función
--    ordenadas :: [Notas] -> [Notas]
-- tal que (ordenadas ns) es la lista de las notas ns ordenadas
-- considerando primero la nota del examen 2, a continuación la del
-- examen 1 y finalmente el nombre. Por ejemplo,
--    λ> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7]
--    [Notas "Juan" 6 5,Notas "Luis" 3 7]
--    λ> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4]
--    [Notas "Luis" 3 4,Notas "Juan" 6 5]
--    λ> ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4]
--    [Notas "Luis" 7 4,Notas "Juan" 6 5]
--    λ> ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4]
--    [Notas "Juan" 6 4,Notas "Luis" 7 4]
--    λ> ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4]
--    [Notas "Luis" 5 4,Notas "Juan" 6 4]
--    λ> ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4]
--    [Notas "Juan" 5 4,Notas "Luis" 5 4]
--    λ> ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4]
--    [Notas "Eva" 5 4,Notas "Juan" 5 4]
-- ---------------------------------------------------------------------

module Ordenacion_de_estructuras where

import Data.List (sort)
import Test.QuickCheck

data Notas = Notas String Int Int
  deriving (Read, Show, Eq)

-- 1ª solución
ordenadas1 :: [Notas] -> [Notas]
ordenadas1 ns =
  [Notas n x y | (y,x,n) <- sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns]]

-- 2ª solución
ordenadas2 :: [Notas] -> [Notas]
ordenadas2 ns =
  map (\(y,x,n) -> Notas n x y) (sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns])

-- Comprobación de equivalencia
-- ============================

-- notasArbitraria es un generador aleatorio de notas. Por ejemplo,
--    λ> sample notasArbitraria
--    Notas "achjkqruvxy" 3 3
--    Notas "abfgikmptuvy" 10 10
--    Notas "degjmptvwx" 7 9
--    Notas "cdefghjmnoqrsuw" 0 9
--    Notas "bcdfikmstuxz" 1 8
--    Notas "abcdhkopqsvwx" 10 7
--    Notas "abghiklnoqstvwx" 0 0
--    Notas "abfghklmnoptuvx" 4 9
--    Notas "bdehjkmpqsxyz" 0 4
--    Notas "afghijmopsvwz" 3 7
--    Notas "bdefghjklnoqx" 2 3
notasArbitraria :: Gen Notas
notasArbitraria = do
  n <- sublistOf ['a'..'z']
  x <- chooseInt (0, 10)
  y <- chooseInt (0, 10)
  return (Notas n x y)

-- Notas es una subclase de Arbitrary
instance Arbitrary Notas where
  arbitrary = notasArbitraria

-- La propiedad es
prop_ordenadas :: [Notas] -> Property
prop_ordenadas ns =
  ordenadas1 ns === ordenadas2 ns

-- La comprobación es
--    λ> quickCheck prop_ordenadas
--    +++ OK, passed 100 tests.
