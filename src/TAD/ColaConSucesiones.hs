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

-- Colas como listas:
newtype Cola a = C (Seq a)
  deriving (Show, Eq)

-- Ejemplo de cola: La cola obtenida añadiéndole a la cola vacía los
-- números del 1 al 10 es
--    λ> foldr inserta vacia [1..10]
--    C [10,9,8,7,6,5,4,3,2,1]

-- vacia es la cola vacía. Por ejemplo,
--    λ> vacia
--    C []
vacia :: Cola a
vacia = C empty

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    λ> inserta 12 (foldr inserta vacia [1..10])
--    C [10,9,8,7,6,5,4,3,2,1,12]
inserta :: a -> Cola a -> Cola a
inserta x (C xs) = C (xs |> x )

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    primero (foldr inserta vacia [1..10])  ==  10
primero :: Cola a -> a
primero (C xs) = case viewl xs of
  EmptyL -> error "primero de la pila vacia"
  x :< _ -> x

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    λ> resto (foldr inserta vacia [1..10])
--    C [9,8,7,6,5,4,3,2,1]
resto :: Cola a -> Cola a
resto (C xs) = case viewl xs of
  EmptyL   -> error "resto la pila vacia"
  _ :< xs' -> C xs'

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia (foldr inserta vacia [1..10]) == False
--    esVacia vacia  == True
esVacia :: Cola a -> Bool
esVacia (C xs)  = S.null xs

-- Generador de colas                                          --
-- ==================

-- genCola es un generador de colas de enteros. Por ejemplo,
--    λ> sample genCola
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([7,8,4,3,7],[5,3,3])
--    C ([],[])
--    C ([1],[13])
--    C ([18,28],[12,21,28,28,3,18,14])
--    C ([47],[64,45,7])
--    C ([8],[])
--    C ([42,112,178,175,107],[])
genCola :: Gen (Cola Int)
genCola = frequency [(1, return vacia),
                     (30, do n <- choose (10,100)
                             xs <- vectorOf n arbitrary
                             return (creaCola xs))]
  where creaCola = foldr inserta vacia

-- El tipo pila es una instancia del arbitrario.
instance Arbitrary (Cola Int) where
  arbitrary = genCola

-- Propiedades de las colas
-- ========================

-- Las propiedades son
prop_colas :: Int -> Int -> Cola Int -> Bool
prop_colas x y c =
  primero (inserta x vacia) == x &&
  primero (inserta x c') == primero c' &&
  resto (inserta x vacia) == vacia &&
  resto (inserta x c') == inserta x (resto c') &&
  esVacia vacia &&
  not (esVacia (inserta x c))
  where c' = inserta y c

-- La comprobación es:
--    λ> quickCheck prop_colas
--    +++ OK, passed 100 tests.
