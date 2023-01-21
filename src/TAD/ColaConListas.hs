-- ColaConListas.hs
-- Implementación de las colas mediante listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-enero-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ColaConListas
  (Cola,
   vacia,   -- Cola a
   inserta, -- a -> Cola a -> Cola a
   primero, -- Cola a -> a
   resto,   -- Cola a -> Cola a
   esVacia, -- Cola a -> Bool
  ) where

import Test.QuickCheck

-- Colas como listas:
newtype Cola a = C [a]
  deriving Eq

-- (escribeCola c) es la cadena correspondiente a la cola c. Por
-- ejemplo,
--    escribeCola (inserta 5 (inserta 2 (inserta 3 vacia))) == "3 | 2 | 5"
escribeCola :: Show a => Cola a -> String
escribeCola (C [])     = "-"
escribeCola (C [x])    = show x
escribeCola (C (x:xs)) = show x ++ " | " ++ escribeCola (C xs)

-- Procedimiento de escritura de colas.
instance Show a => Show (Cola a) where
  show = escribeCola

-- Ejemplo de cola:
--    λ> inserta 5 (inserta 2 (inserta 3 vacia))
--    3 | 2 | 5

-- vacia es la cola vacía. Por ejemplo,
--    λ> vacia
--    -
vacia :: Cola a
vacia = C []

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    λ> ej = inserta 2 (inserta 3 vacia)
--    λ> ej
--    3 | 2
--    λ> inserta 5 ej
--    3 | 2 | 5
inserta :: a -> Cola a -> Cola a
inserta x (C c) = C (c ++ [x])

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    λ> primero (inserta 5 (inserta 2 (inserta 3 vacia)))
--    3
primero :: Cola a -> a
primero (C [])    = error "primero: cola vacia"
primero (C (x:_)) = x

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    λ> resto (inserta 5 (inserta 2 (inserta 3 vacia)))
--    2 | 5
resto :: Cola a -> Cola a
resto (C (_:xs)) = C xs
resto (C [])     = error "resto: cola vacia"

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia (inserta 5 (inserta 2 (inserta 3 vacia))) == False
--    esVacia vacia  == True
esVacia :: Cola a -> Bool
esVacia (C xs)  = null xs

-- Generador de colas                                          --
-- ==================

-- genCola es un generador de colas de enteros. Por ejemplo,
--    λ> sample genCola
--    -
--    -
--    -3 | 2
--    6 | 0 | 1
--    -5 | 0 | -5 | 0 | -4
--    2 | 9 | -6 | 9 | 0 | -1
--    -
--    11 | -5 | 5
--    -
--    16 | 6 | 15 | -3 | -9
--    11 | 6 | 15 | 13 | 20 | -7 | 11 | -5 | 13
genCola :: (Arbitrary a, Num a) => Gen (Cola a)
genCola = do
  xs <- listOf arbitrary
  return (foldr inserta vacia xs)

-- El tipo pila es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Cola a) where
  arbitrary = genCola

-- Propiedades de las colas
-- ========================

-- Las propiedades son
prop_colas1 :: Int -> Cola Int -> Bool
prop_colas1 x c =
  primero (inserta x vacia) == x &&
  resto (inserta x vacia) == vacia &&
  esVacia vacia &&
  not (esVacia (inserta x c))

prop_colas2 :: Int -> Cola Int -> Property
prop_colas2 x c =
  not (esVacia c) ==>
  primero (inserta x c) == primero c &&
  resto (inserta x c) == inserta x (resto c)

-- La comprobación es:
--    λ> quickCheck prop_colas1
--    +++ OK, passed 100 tests.
--    λ> quickCheck prop_colas2
--    +++ OK, passed 100 tests; 3 discarded.
