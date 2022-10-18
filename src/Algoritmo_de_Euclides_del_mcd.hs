-- Algoritmo_de_Euclides_del_mcd.hs
-- Algoritmo de Euclides del mcd.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dados dos números naturales, a y b, es posible calcular su máximo
-- común divisor mediante el Algoritmo de Euclides. Este algoritmo se
-- puede resumir en la siguiente fórmula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a módulo b), si b > 0
--
-- Definir la función
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
--
-- Comprobar con QuickCheck que el máximo común divisor de dos números a
-- y b (ambos mayores que 0) es siempre mayor o igual que 1 y además es
-- menor o igual que el menor de los números a  y b.
-- ---------------------------------------------------------------------

module Algoritmo_de_Euclides_del_mcd where

import Test.QuickCheck

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- La propiedad es
prop_mcd :: Positive Integer -> Positive Integer -> Bool
prop_mcd (Positive a) (Positive b) =
  m >= 1 && m <= min a b
  where m = mcd a b

-- La comprobación es
--    λ> quickCheck prop_mcd
--    OK, passed 100 tests.
