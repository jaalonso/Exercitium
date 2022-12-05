-- Interpretaciones_de_una_formula.hs
-- El tipo de las fórmulas proposicionales: Interpretaciones de una fórmula
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 02-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El tipo de las fórmulas proposicionales se puede definir por
--    data FProp = Const Bool
--               | Var Char
--               | Neg FProp
--               | Conj FProp FProp
--               | Impl FProp FProp
--      deriving Show
-- de modo que la fórmula A → ⊥ ∧ ¬B se representa por
--    Impl (Var 'A') (Conj (Const False) (Neg (Var 'B')))
--
-- Una interpretación de una fórmula es una función de sus variables en
-- los booleanos. Por ejemplo, la interpretación que a la variable A le
-- asigna verdadero y a la B falso se puede representar por
--    [('A', True), ('B', False)]
--
-- El tipo de las intepretaciones de puede definir por
--    type Interpretacion = [(Char, Bool)]
--
-- Definir la función
--    interpretaciones :: FProp -> [Interpretacion]
-- tal que (interpretaciones p) es la lista de las interpretaciones de
-- la fórmula p. Por ejemplo,
--    λ> interpretaciones (Impl (Var 'A') (Conj (Var 'A') (Var 'B')))
--    [[('A',False),('B',False)],
--     [('A',False),('B',True)],
--     [('A',True),('B',False)],
--     [('A',True),('B',True)]]
-- ---------------------------------------------------------------------

module Interpretaciones_de_una_formula where

import Data.List (nub)

data FProp = Const Bool
           | Var Char
           | Neg FProp
           | Conj FProp FProp
           | Impl FProp FProp
  deriving Show

type Interpretacion = [(Char, Bool)]

interpretaciones :: FProp -> [Interpretacion]
interpretaciones p =
  [zip vs i | i <- interpretacionesVar (length vs)]
  where vs = nub (variables p)

-- (interpretacionesVar n) es la lista de las interpretaciones de n
-- variables. Por ejemplo,
--    λ> interpretacionesVar 2
--    [[False,False],
--     [False,True],
--     [True,False],
--     [True,True]]
interpretacionesVar :: Int -> [[Bool]]
interpretacionesVar 0 = [[]]
interpretacionesVar n = map (False:) bss ++ map (True:) bss
  where bss = interpretacionesVar (n-1)

-- (variables p) es la lista de las variables de la fórmula p. Por
-- ejemplo,
--    λ> variables (Impl (Var 'A') (Conj (Const False) (Neg (Var 'B'))))
--    "AB"
--    λ> variables (Impl (Var 'A') (Conj (Var 'A') (Neg (Var 'B'))))
--    "AAB"
variables :: FProp -> [Char]
variables (Const _)  = []
variables (Var x)    = [x]
variables (Neg p)    = variables p
variables (Conj p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q
