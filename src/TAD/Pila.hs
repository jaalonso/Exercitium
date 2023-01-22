-- Pila.hs
-- El tipo abstracto de datos de las pilas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-enero-2023
-- ---------------------------------------------------------------------

-- Una pila es una estructura de datos, caracterizada por ser una
-- secuencia de elementos en la que las operaciones de inserción y
-- extracción se realizan por el mismo extremo.
--
-- Las operaciones que definen a tipo abstracto de datos (TAD) de las
-- pilas (cuyos elementos son del tipo a) son las siguientes:
--    vacia    :: Pila a
--    apila    :: a -> Pila a -> Pila a
--    cima     :: Pila a -> a
--    desapila :: Pila a -> Pila a
--    esVacia  :: Pila a -> Bool
-- tales que
--    + vacia es la pila vacía.
--    + (apila x p) es la pila obtenida añadiendo x al principio de p.
--    + (cima p) es la cima de la pila p.
--    + (desapila p) es la pila obtenida suprimiendo la cima de p.
--    + (esVacia p) se verifica si p es la pila vacía.
--
-- Las operaciones tienen que verificar las siguientes propiedades:
--    + cima(apila(x, p) == x
--    + desapila(apila(x, p)) == p
--    + esVacia(vacia)
--    + not esVacia(apila(x, p))
--
-- Para usar el TAD hay que usar una implementación concreta. En
-- principio, consideraremos dos una usando listas y otra usando
-- sucesiones. Hay que elegir la que se desee utilizar, descomentándola
-- y comentando las otras.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD.Pila
  (Pila,
   vacia,      -- Pila a
   apila,      -- a -> Pila a -> Pila a
   cima,       -- Pila a -> a
   desapila,   -- Pila a -> Pila a
   esVacia,    -- Pila a -> Bool
  ) where

import TAD.PilaConListas
-- import TAD.PilaConSucesiones
