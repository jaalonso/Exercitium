-- ColaConListas.hs
-- Implementación de las colas mediante listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-enero-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ColaConListas
  (Cola,
   vacia,   -- Cola a
   inserta, -- a -> Cola a -> Cola a
   primero, -- Cola a -> a
   resto,   -- Cola a -> Cola a
   esVacia, -- Cola a -> Bool
   valida   -- Cola a -> Bool
  ) where

import Test.QuickCheck

-- Colas como listas:
newtype Cola a = C [a]
  deriving (Show, Eq)

-- Ejemplo de cola: La cola obtenida añadiéndole a la cola vacía los
-- números del 1 al 10 es
--    λ> foldr inserta vacia [1..10]
--    C [10,9,8,7,6,5,4,3,2,1]

-- vacia es la cola vacía. Por ejemplo,
--    λ> vacia
--    C []
vacia :: Cola a
vacia = C []

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    λ> inserta 12 (foldr inserta vacia [1..10])
--    C [10,9,8,7,6,5,4,3,2,1,12]
inserta :: a -> Cola a -> Cola a
inserta x (C c) = C (c ++ [x])

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    primero (foldr inserta vacia [1..10])  ==  10
primero :: Cola a -> a
primero (C (x:_)) = x
primero (C [])    = error "primero: cola vacia"

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    λ> resto (foldr inserta vacia [1..10])
--    C [9,8,7,6,5,4,3,2,1]
resto :: Cola a -> Cola a
resto (C (_:xs)) = C xs
resto (C [])     = error "resto: cola vacia"

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia (foldr inserta vacia [1..10]) == False
--    esVacia vacia  == True
esVacia :: Cola a -> Bool
esVacia (C xs)  = null xs

-- (valida c) se verifica si c representa una cola válida. Con esta
-- representación, todas las colas son válidas.
valida :: Cola a -> Bool
valida _ = True

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

-- Propiedades de la normalización                                    --
-- ===============================

-- Propiedad. La cola vacía es válida.
prop_valida_vacia :: Bool
prop_valida_vacia = valida vacia

-- Comprobación
--    λ> quickCheck prop_valida_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. Al añadirle un elemento a una cola válida se obtiene otra
-- cola válida.
prop_valida_inserta :: Cola Int -> Int -> Property
prop_valida_inserta c x =
  valida c ==> valida (inserta x c)

-- Comprobación.
--    λ> quickCheck prop_valida_inserta
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de una cola válida y no vacía es una cola válida.
prop_valida_resto :: Cola Int -> Property
prop_valida_resto c =
  valida c && not (esVacia c) ==> valida (resto c)

-- Comprobación
--    λ> quickCheck prop_valida_resto
--    +++ OK, passed 100 tests.

-- Propiedad. Todo los elementos generados por genCola son colas
-- válidas.
prop_genCola_correcto :: Cola Int -> Bool
prop_genCola_correcto = valida

-- Comprobación.
--    λ> quickCheck prop_genCola_correcto
--    +++ OK, passed 100 tests.

-- Propiedades de las colas
-- ========================

-- Propiedad. El primero de la cola obtenida añadiendo x a la cola vacía
-- es x.
prop_primero_inserta_vacia :: Int -> Bool
prop_primero_inserta_vacia x =
  primero (inserta x vacia) == x

-- Comprobación.
--    λ> quickCheck prop_primero_inserta_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. Si una cola no está vacía, su primer elemento no varía al
-- añadirle un elemento.
prop_primero_inserta_no_vacia :: Cola Int -> Int -> Int -> Bool
prop_primero_inserta_no_vacia c x y =
  primero (inserta x c') == primero c'
  where c' = inserta y c

-- Comprobación.
--    λ> quickCheck prop_primero_inserta_no_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de la cola obtenida insertando un elemento en la
-- cola vacía es la cola vacía.
prop_resto_inserta_vacia :: Int -> Bool
prop_resto_inserta_vacia x =
  resto (inserta x vacia) == vacia

-- Comprobación.
--    λ> quickCheck prop_resto_inserta_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. Las operaciones de inserta y resto conmutan.
prop_resto_inserta_en_no_vacia :: Cola Int -> Int -> Int -> Bool
prop_resto_inserta_en_no_vacia c x y =
  resto (inserta x c') == inserta x (resto c')
  where c' = inserta y c

-- Comprobación.
--    λ> quickCheck prop_resto_inserta_en_no_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. vacia es una cola vacía.
prop_vacia_es_vacia :: Bool
prop_vacia_es_vacia =
  esVacia vacia

-- Comprobación.
--    λ> quickCheck prop_vacia_es_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. La cola obtenida insertando un elemento no es vacía.
prop_inserta_no_es_vacia :: Int -> Cola Int -> Bool
prop_inserta_no_es_vacia x c =
  not (esVacia (inserta x c))

-- Comprobación
--    λ> quickCheck prop_inserta_no_es_vacia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaCola :: IO Bool
verificaCola = $quickCheckAll

-- La verificación es
--    λ> verificaCola
--    === prop_valida_vacia from ColaConListas.hs:101 ===
--    +++ OK, passed 1 test.
--
--    === prop_valida_inserta from ColaConListas.hs:110 ===
--    +++ OK, passed 100 tests.
--
--    === prop_valida_resto from ColaConListas.hs:119 ===
--    +++ OK, passed 100 tests; 3 discarded.
--
--    === prop_genCola_correcto from ColaConListas.hs:129 ===
--    +++ OK, passed 100 tests.
--
--    === prop_primero_inserta_vacia from ColaConListas.hs:141 ===
--    +++ OK, passed 100 tests.
--
--    === prop_primero_inserta_no_vacia from ColaConListas.hs:151 ===
--    +++ OK, passed 100 tests.
--
--    === prop_resto_inserta_vacia from ColaConListas.hs:162 ===
--    +++ OK, passed 100 tests.
--
--    === prop_resto_inserta_en_no_vacia from ColaConListas.hs:171 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacia_es_vacia from ColaConListas.hs:181 ===
--    +++ OK, passed 1 test.
--
--    === prop_inserta_no_es_vacia from ColaConListas.hs:190 ===
--    +++ OK, passed 100 tests.
--
--    True
