-- GenPilaConListas.hs
-- Generados de pilas representadas con listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-enero-2023
-- ---------------------------------------------------------------------

module GenPilaConListas where

import PilaConListas
import Test.QuickCheck

-- Generador de pilas                                          --
-- ==================

-- genPila es un generador de pilas. Por ejemplo,
--    λ> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do
  xs <- listOf arbitrary
  return (foldr apila vacia xs)

-- El tipo pila es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
  arbitrary = genPila

-- Propiedades
-- ===========

-- Las propiedades son
prop_pilas :: Int -> Pila Int -> Bool
prop_pilas x p =
  cima (apila x p) == x &&
  desapila (apila x p) == p &&
  esVacia vacia &&
  not (esVacia (apila x p))

-- La comprobación e:
--    λ> quickCheck prop_pilas
--    +++ OK, passed 100 tests.
