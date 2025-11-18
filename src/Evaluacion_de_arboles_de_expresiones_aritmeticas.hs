-- Evaluacion_de_arboles_de_expresiones_aritmeticas.hs
-- Evaluación de árboles de expresiones aritméticas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-Noviembre-2014 (actualizado 18-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las expresiones aritméticas se pueden representar como árboles con
-- números en las hojas y operaciones en los nodos. Por ejemplo, la
-- expresión "9-2*4" se puede representar por el árbol
--      -
--     / \
--    9   *
--       / \
--      2   4
--
-- Definiendo el tipo de dato Arbol por
--    data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol
-- la representación del árbol anterior es
--    N (-) (H 9) (N (*) (H 2) (H 4))
--
-- Definir la función
--    valor :: Arbol -> Int
-- tal que (valor a) es el valor de la expresión aritmética
-- correspondiente al árbol a. Por ejemplo,
--    valor (N (-) (H 9) (N (*) (H 2) (H 4)))  ==  1
--    valor (N (+) (H 9) (N (*) (H 2) (H 4)))  ==  17
--    valor (N (+) (H 9) (N div (H 4) (H 2)))  ==  11
--    valor (N (+) (H 9) (N max (H 4) (H 2)))  ==  13
-- ---------------------------------------------------------------------

module Evaluacion_de_arboles_de_expresiones_aritmeticas where

import Test.Hspec (Spec, hspec, it, shouldBe)

data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol

valor :: Arbol -> Int
valor (H x)     = x
valor (N f i d) = f (valor i) (valor d)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    valor (N (-) (H 9) (N (*) (H 2) (H 4)))    `shouldBe`  1
  it "e2" $
    valor (N (+) (H 9) (N (*) (H 2) (H 4)))    `shouldBe`  17
  it "e3" $
    valor (N (+) (H 9) (N div (H 4) (H 2)))  `shouldBe`  11
  it "e4" $
    valor (N (+) (H 9) (N max (H 4) (H 2)))  `shouldBe`  13

-- La verificación es
--    λ> verifica
--    3 examples, 0 failures
