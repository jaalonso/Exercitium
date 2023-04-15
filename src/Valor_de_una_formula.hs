-- Valor_de_una_formula.hs
-- El tipo de las fórmulas proposicionales: Valor de una fórmula
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 01-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una interpretación de una fórmula es una función de sus variables en
-- los booleanos. Por ejemplo, la interpretación que a la variable A le
-- asigna verdadero y a la B falso se puede representar por
--    [('A', True), ('B', False)]
--
-- El tipo de las intepretaciones de puede definir por
--    type Interpretacion = [(Char, Bool)]
--
-- El valor de una fórmula en una interpretación se calcula usando las
-- funciones de verdad de las conectivas que se muestran a continuación
--    |---+----|   |---+---+-------+-------|
--    | p | ¬p |   | p | q | p ∧ q | p → q |
--    |---+----|   |---+---+-------+-------|
--    | T | F  |   | T | T | T     | T     |
--    | F | T  |   | T | F | F     | F     |
--    |---+----|   | F | T | F     | T     |
--                 | F | F | F     | T     |
--                 |---+---+-------+-------|
--
-- Usando el tipo de las fórmulas proposicionales definido en el
-- [ejercicio anterior](https://bit.ly/3L3G2SX), definir la función
--    valor :: Interpretacion -> FProp -> Bool
-- tal que (valor i p) es el valor de la fórmula p en la interpretación
-- i. Por ejemplo,
--    λ> p = Impl (Var 'A') (Conj (Var 'A') (Var 'B'))
--    λ> valor [('A',False),('B',False)] p
--    True
--    λ> valor [('A',True),('B',False)] p
--    False
-- ---------------------------------------------------------------------

module Valor_de_una_formula where

import Tipo_de_formulas (FProp(..))

type Interpretacion = [(Char, Bool)]

valor :: Interpretacion -> FProp -> Bool
valor _ (Const b)  = b
valor i (Var x)    = busca x i
valor i (Neg p)    = not (valor i p)
valor i (Conj p q) = valor i p && valor i q
valor i (Impl p q) = valor i p <= valor i q

-- (busca c t) es el valor del primer elemento de la lista de asociación
-- t cuya clave es c. Por ejemplo,
--    busca 2 [(1,'a'),(3,'d'),(2,'c')]  ==  'c'
busca :: Eq c => c -> [(c,v)] -> v
busca c t = head [v | (c',v) <- t, c == c']
