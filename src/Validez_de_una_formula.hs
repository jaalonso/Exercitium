-- Validez_de_una_formula.hs
-- Reconocedor de tautologías.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-diciembre-2022
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
-- Una fórmula es una tautología si es verdadera en todas sus
-- interpretaciones. Por ejemplo,
-- + (A ∧ B) → A es una tautología
-- + A → (A ∧ B) no es una tautología
--
-- Definir la función
--    esTautologia :: FProp -> Bool
-- tal que (esTautologia p) se verifica si la fórmula p es una
-- tautología. Por ejemplo,
--    λ> esTautologia (Impl (Conj (Var 'A') (Var 'B')) (Var 'A'))
--    True
--    λ> esTautologia (Impl (Var 'A') (Conj (Var 'A') (Var 'B')))
--    False
-- ---------------------------------------------------------------------

module Validez_de_una_formula where

import Data.List (nub)

data FProp = Const Bool
           | Var Char
           | Neg FProp
           | Conj FProp FProp
           | Impl FProp FProp
  deriving Show

type Interpretacion = [(Char, Bool)]

esTautologia :: FProp -> Bool
esTautologia p =
  and [valor i p | i <- interpretaciones p]

-- (valor i p) es el valor de la fórmula p en la interpretación i. Por
-- ejemplo,
--    λ> p = Impl (Var 'A') (Conj (Var 'A') (Var 'B'))
--    λ> valor [('A',False),('B',False)] p
--    True
--    λ> valor [('A',True),('B',False)] p
--    False
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
