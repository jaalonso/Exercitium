-- ContenidaCola.hs
-- TAD de colas: Inclusión de colas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    contenidaCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (contenidaCola c1 c2) se verifica si todos los elementos de
-- de la cola c1 son elementos de la cola c2. Por ejemplo,
--    λ> ej1 = inserta 3 (inserta 2 vacia)
--    λ> ej2 = inserta 3 (inserta 4 vacia)
--    λ> ej3 = inserta 5 (inserta 2 (inserta 3 vacia))
--    λ> contenidaCola ej1 ej3
--    True
--    λ> contenidaCola ej2 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ContenidaCola where

import TAD.Cola (Cola, vacia, inserta, esVacia, primero, resto)
import PerteneceCola (perteneceCola)
import Transformaciones_colas_listas (colaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

contenidaCola1 :: Eq a => Cola a -> Cola a -> Bool
contenidaCola1 c1 c2
  | esVacia c1 = True
  | otherwise  = perteneceCola (primero c1) c2 &&
                 contenidaCola1 (resto c1) c2

-- La función perteneceCola está definida en el ejercicio
-- "TAD de las colas: Pertenencia a una cola" que se encuentra en
-- https://bit.ly/3RcVgqb

-- 2ª solución
-- ===========

contenidaCola2 :: Eq a => Cola a -> Cola a -> Bool
contenidaCola2 c1 c2 =
  contenidaLista (colaAlista c1) (colaAlista c2)

-- La función colaAlista está definida en el ejercicio
-- "TAD de las colas: Transformaciones entre colas y listas" que se
-- encuentra en https://bit.ly/3Xv0oIt

contenidaLista :: Eq a => [a] -> [a] -> Bool
contenidaLista xs ys =
  all (`elem` ys) xs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_contenidaCola :: Cola Int -> Cola Int -> Bool
prop_contenidaCola c1 c2 =
  contenidaCola1 c1 c2 == contenidaCola2 c1 c2

-- La comprobación es
--    λ> quickCheck prop_contenidaCola
--    +++ OK, passed 100 tests.
