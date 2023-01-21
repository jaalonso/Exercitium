-- ColaConSucesiones.hs
-- Implementación de las colas mediante sucesiones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-enero-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ColaConSucesiones
  (Cola,
   vacia,   -- Cola a
   inserta, -- a -> Cola a -> Cola a
   primero, -- Cola a -> a
   resto,   -- Cola a -> Cola a
   esVacia, -- Cola a -> Bool
  ) where

import Data.Sequence as S
import Test.QuickCheck

-- Colas como sucesiones:
newtype Cola a = C (Seq a)
  deriving Eq

-- (escribeCola c) es la cadena correspondiente a la cola c. Por
-- ejemplo,
--    escribeCola (inserta 5 (inserta 2 (inserta 3 vacia))) == "3 | 2 | 5"
escribeCola :: Show a => Cola a -> String
escribeCola (C xs) = case viewl xs of
    EmptyL   -> "-"
    x :< xs' -> case viewl xs' of
        EmptyL -> show x
        _      -> show x ++ " | " ++ escribeCola (C xs')

-- Procedimiento de escritura de colas.
instance Show a => Show (Cola a) where
  show = escribeCola

-- Ejemplo de cola:
--    λ> inserta 5 (inserta 2 (inserta 3 vacia))
--    3 | 2 | 5

-- vacia es la cola vacía. Por ejemplo,
--    λ> vacia
--    C []
vacia :: Cola a
vacia = C empty

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    λ> ej = inserta 2 (inserta 3 vacia)
--    λ> ej
--    3 | 2
--    λ> inserta 5 ej
--    3 | 2 | 5
inserta :: a -> Cola a -> Cola a
inserta x (C xs) = C (xs |> x )

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    λ> primero (inserta 5 (inserta 2 (inserta 3 vacia)))
--    3
primero :: Cola a -> a
primero (C xs) = case viewl xs of
  EmptyL -> error "primero de la pila vacia"
  x :< _ -> x

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    λ> resto (inserta 5 (inserta 2 (inserta 3 vacia)))
--    2 | 5
resto :: Cola a -> Cola a
resto (C xs) = case viewl xs of
  EmptyL   -> error "resto la pila vacia"
  _ :< xs' -> C xs'

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia (inserta 5 (inserta 2 (inserta 3 vacia))) == False
--    esVacia vacia  == True
esVacia :: Cola a -> Bool
esVacia (C xs)  = S.null xs

-- Generador de colas                                          --
-- ==================

-- genCola es un generador de colas de enteros. Por ejemplo,
--    λ> sample genCola
--    -
--    2 | -2
--    0 | 0 | 0 | 4
--    -
--    2
--    -1 | -6 | 9
--    12 | -12 | -12 | 7 | -2 | -3 | 5 | -8 | -3 | -9 | -6
--    -11 | -5 | -7 | -8 | -10 | 8 | -9 | -7 | 6 | -12 | 8 | -9 | -1
--    -16 | -12
--    -17 | -17 | 1 | 2 | -15 | -15 | -13 | 8 | 13 | -12 | 15
--    -16 | -18
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
--    +++ OK, passed 100 tests; 9 discarded.
