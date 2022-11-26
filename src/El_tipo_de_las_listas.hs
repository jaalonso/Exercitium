-- El_tipo_de_las_listas.hs
-- El tipo de las listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El tipo de las listas, con elementos de tipo a, se puede definir por
--    data Lista a = Nil | Cons a (Lista a)
-- Por ejemplo, la lista [4,2,5] se representa por
-- Cons 4 (Cons 2 (Cons 5 Nil)).
--
-- Definir la función
--    longitud :: Lista a -> Int
-- tal que  (longitud xs) es la longitud de la lista xs. Por ejemplo,
--    longitud (Cons 4 (Cons 2 (Cons 5 Nil)))  ==  3
-- ---------------------------------------------------------------------

module El_tipo_de_las_listas where

data Lista a = Nil | Cons a (Lista a)

longitud :: Lista a -> Int
longitud Nil         = 0
longitud (Cons _ xs) = 1 + longitud xs
