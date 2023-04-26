-- Expresiones_aritmeticas_reducibles.hs
-- Expresiones aritméticas reducibles.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas con variables]
-- (https://bit.ly/3HfB0QO), definir la función
--    reducible :: Expr -> Bool
-- tal que (reducible a) se verifica si a es una expresión reducible; es
-- decir, contiene una operación en la que los dos operandos son números.
-- Por ejemplo,
--    reducible (S (C 3) (C 4))             == True
--    reducible (S (C 3) (V 'x'))           == False
--    reducible (S (C 3) (P (C 4) (C 5)))   == True
--    reducible (S (V 'x') (P (C 4) (C 5))) == True
--    reducible (S (C 3) (P (V 'x') (C 5))) == False
--    reducible (C 3)                       == False
--    reducible (V 'x')                     == False
-- ---------------------------------------------------------------------

module Expresiones_aritmeticas_reducibles where

import Expresion_aritmetica_con_variables (Expr (C, V, S, P))

reducible :: Expr -> Bool
reducible (C _)           = False
reducible (V _)           = False
reducible (S (C _) (C _)) = True
reducible (S a b)         = reducible a || reducible b
reducible (P (C _) (C _)) = True
reducible (P a b)         = reducible a || reducible b
