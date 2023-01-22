-- UltimoCola.hs
-- TAD de las colas: Último elemento
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    ultimoCola :: Cola a -> a
-- tal que (ultimoCola c) es el último elemento de la cola c. Por
-- ejemplo:
--    ultimoCola ejCola4 == 4
--    ultimoCola ejCola5 == 15
-- ---------------------------------------------------------------------

module UltimoCola where

import TAD.Cola

ultimoCola :: Cola a -> a
ultimoCola c
  | esVacia c  = error "cola vacia"
  | esVacia rc = pc
  | otherwise  = ultimoCola rc
  where pc = primero c
        rc = resto c
