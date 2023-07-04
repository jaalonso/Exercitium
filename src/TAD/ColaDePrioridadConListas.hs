-- ColaDePrioridadConListas.hs
-- El tipo de datos de las colas de prioridad mediante listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-julio-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ColaDePrioridadConListas
  (CPrioridad,
   vacia,   -- Ord a => CPrioridad a
   inserta, -- Ord a => a -> CPrioridad a -> CPrioridad a
   primero, -- Ord a => CPrioridad a -> a
   resto,   -- Ord a => CPrioridad a -> CPrioridad a
   esVacia, -- Ord a => CPrioridad a -> Bool
   valida   -- Ord a => CPrioridad a -> Bool
  ) where

import Test.QuickCheck

-- Colas de prioridad mediante listas.
newtype CPrioridad a = CP [a]
  deriving Eq

-- (escribeColaDePrioridad c) es la cadena correspondiente a la cola de
-- prioridad c. Por ejemplo,
--    λ> escribeColaDePrioridad (inserta 5 (inserta 2 (inserta 3 vacia)))
--    "2 | 3 | 5"
escribeColaDePrioridad :: Show a => CPrioridad a -> String
escribeColaDePrioridad (CP [])     = "-"
escribeColaDePrioridad (CP [x])    = show x
escribeColaDePrioridad (CP (x:xs)) = show x ++ " | " ++ escribeColaDePrioridad (CP xs)

-- Procedimiento de escritura de colas de prioridad.
instance Show a => Show (CPrioridad a) where
  show = escribeColaDePrioridad

-- Ejemplo de cola de prioridad
--    λ> inserta 5 (inserta 2 (inserta 3 vacia))
--    2 | 3 | 5

-- (valida c) se verifica si c es una cola de prioridad válida. Por
-- ejemplo,
--    valida (CP [1,3,5])  ==  True
--    valida (CP [1,5,3])  ==  False
valida :: Ord a => CPrioridad a -> Bool
valida (CP xs) = ordenada xs
  where ordenada (x:y:zs) = x <= y && ordenada (y:zs)
        ordenada _        = True

-- vacia es la cola de prioridad vacía. Por ejemplo,
--    λ> vacia
--    CP []
vacia :: Ord a => CPrioridad a
vacia = CP []

-- (inserta x c) es la cola obtenida añadiendo el elemento x a la cola
-- de prioridad c. Por ejemplo,
--    λ> inserta 5 (foldr inserta vacia [3,1,7,2,9])
--    1 | 2 | 3 | 5 | 7 | 9
inserta :: Ord a => a -> CPrioridad a -> CPrioridad a
inserta x (CP q) = CP (ins x q)
  where ins y []                   = [y]
        ins y r@(e:r') | y < e     = y:r
                       | otherwise = e:ins y r'

-- (primero c) es el primer elemento de la cola de prioridad c. Por
-- ejemplo,
--    primero (foldr inserta vacia [3,1,7,2,9])  ==  1
primero :: Ord a => CPrioridad a -> a
primero (CP(x:_)) = x
primero _         = error "primero: cola de prioridad vacia"

-- (resto c) es la cola de prioridad obtenida eliminando el primer
-- elemento de la cola de prioridad c. Por ejemplo,
--    λ> resto (foldr inserta vacia [3,1,7,2,9])
--    2 | 3 | 7 | 9
resto :: Ord a => CPrioridad a -> CPrioridad a
resto (CP (_:xs)) = CP xs
resto _           = error "resto: cola de prioridad vacia"

-- (esVacia c) se verifica si la cola de prioridad c es vacía. Por
-- ejemplo,
--    esVacia (foldr inserta vacia [3,1,7,2,9]) ==  False
--    esVacia vacia                             ==  True
esVacia :: Ord a => CPrioridad a -> Bool
esVacia (CP xs) = null xs

-- Generador de colas de prioridad
-- ===============================

-- genCPrioridad es un generador de colas de enteros. Por ejemplo,
--    λ> sample genCPrioridad
--    -
--    0 | 0
--    4
--    -4 | -3 | 6 | 6
--    -7 | -6 | -2 | 0
--    -10 | -10 | -5 | 1 | 4 | 6 | 6 | 9 | 10
--    -
--    -13 | -11 | -9 | -5 | -2 | -1 | 0 | 1 | 2 | 2 | 13 | 14
--    -15 | -13 | -13 | -5 | -3 | -1 | 3 | 5 | 7 | 9 | 9 | 14 | 16
--    -
--    -17 | -15 | -14 | -5 | -2 | 1 | 1 | 2 | 5 | 7
genCPrioridad :: (Arbitrary a, Num a, Ord a) =>  Gen (CPrioridad a)
genCPrioridad = do
  xs <- listOf arbitrary
  return (foldr inserta vacia xs)

-- El tipo cola de prioridad es una instancia del arbitrario.
instance (Arbitrary a, Num a, Ord a) => Arbitrary (CPrioridad a) where
  arbitrary = genCPrioridad

-- Prop.: Las colas de prioridad producidas por genCPrioridad son
-- válidas.
prop_genCPrioridad_correcto ::  CPrioridad Int -> Bool
prop_genCPrioridad_correcto = valida

-- Comprobación.
--    λ> quickCheck prop_genCPrioridad_correcto
--    +++ OK, passed 100 tests.

-- Propiedades de las colas de prioridad
-- =====================================

-- Propiedad. Si se añade dos elementos a una cola de prioridad se
-- obtiene la misma cola de prioridad idependientemente del orden en
-- que se añadan los elementos.
prop_inserta_conmuta :: Int -> Int -> CPrioridad Int -> Bool
prop_inserta_conmuta x y c =
  inserta x (inserta y c) == inserta y (inserta x c)

-- Comprobación.
--    λ> quickCheck prop_inserta_conmuta
--    +++ OK, passed 100 tests.

-- Propiedad. La cabeza de la cola de prioridad obtenida añadiendo un
-- elemento x a la cola de prioridad vacía es x.
prop_primero_inserta_vacia :: Int -> CPrioridad Int -> Bool
prop_primero_inserta_vacia x _ =
  primero (inserta x vacia) == x

-- Comprobación.
--    λ> quickCheck prop_primero_inserta_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. El primer elemento de una cola de prioridad c no cambia
-- cuando se le añade un elemento mayor o igual que algún elemento de c.
prop_primero_inserta :: Int -> Int -> CPrioridad Int -> Property
prop_primero_inserta x y c =
  x <= y ==> primero (inserta y c') == primero c'
  where c' = inserta x c

-- Comprobación.
--    λ> quickCheck prop_primero_inserta
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de añadir un elemento a la cola de prioridad
-- vacía es la cola vacía.
prop_resto_inserta_vacia :: Int -> Bool
prop_resto_inserta_vacia x =
  resto (inserta x vacia) == vacia

-- Comprobación.
--    λ> quickCheck prop_resto_inserta_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de la cola de prioridad obtenida añadiendo un
-- elemento y a una cola c' (que tiene algún elemento menor o igual que
-- y) es la cola que se obtiene añadiendo y al resto de c'.
prop_resto_inserta :: Int -> Int -> CPrioridad Int -> Property
prop_resto_inserta x y c =
  x <= y ==> resto (inserta y c') == inserta y (resto c')
  where c' = inserta x c

-- Comprobación:
--    λ> quickCheck prop_resto_inserta
--    +++ OK, passed 100 tests.

-- Propiedad. vacia es una cola vacía.
prop_vacia_es_vacia :: Bool
prop_vacia_es_vacia = esVacia (vacia :: CPrioridad Int)

-- Comprobación.
--    λ> quickCheck prop_vacia_es_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. Si se añade un elemento a una cola de prioridad se obtiene
-- una cola no vacía.
prop_inserta_no_es_vacia :: Int -> CPrioridad Int -> Bool
prop_inserta_no_es_vacia x c =
  not (esVacia (inserta x c))

-- Comprobación.
--    λ> quickCheck prop_inserta_no_es_vacia
--    +++ OK, passed 100 tests.
