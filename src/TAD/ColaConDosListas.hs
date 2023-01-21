-- ColaConDosListas.hs
-- Implementación de las colas mediante dos listas.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

-- En esta implementación, una cola c se representa mediante un par de
-- listas (xs,ys) de modo que los elementos de c son, en ese orden, los
-- elementos de la lista xs++(reverse ys).
--
-- Al dividir la lista en dos parte e invertir la segunda de ellas,
-- esperamos hacer más eficiente las operaciones sobre las colas.
--
-- Impondremos también una restricción adicional sobre la
-- representación: las colas serán representadas mediante pares (xs,ys)
-- tales que si xs es vacía, entonces ys será también vacía. Esta
-- restricción ha de ser conservada por los programas que crean colas.

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ColaConDosListas
  (Cola,
   vacia,      -- Cola a
   inserta,    -- a -> Cola a -> Cola a
   primero,    -- Cola a -> a
   resto,      -- Cola a -> Cola a
   esVacia,    -- Cola a -> Bool
  ) where

import Test.QuickCheck

-- Las colas como pares listas.
newtype Cola a = C ([a],[a])

-- (escribeCola p) es la cadena correspondiente a la cola p. Por
-- ejemplo,
--    λ> escribeCola (inserta 5 (inserta 2 (inserta 3 vacia)))
--    "3 | 2 | 5"
escribeCola :: Show a => Cola a -> String
escribeCola (C (xs,ys)) = aux (xs ++ reverse ys)
  where aux []     = "-"
        aux [z]    = show z
        aux (z:zs) = show z ++ " | " ++ aux zs

-- Procedimiento de escritura de colas.
instance Show a => Show (Cola a) where
  show = escribeCola

-- Ejemplo de cola:
--    λ> inserta 5 (inserta 2 (inserta 3 vacia))
--    3 | 2 | 5

-- vacia es la cola vacía. Por ejemplo,
--    λ>  vacia
--    -
vacia :: Cola a
vacia  = C ([],[])

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    λ> inserta 5 (inserta 2 (inserta 3 vacia))
--    3 | 2 | 5
inserta :: a -> Cola a -> Cola a
inserta y (C (xs,ys)) = C (normaliza (xs,y:ys))

-- (normaliza p) es la cola obtenida al normalizar el par de listas
-- p. Por ejemplo,
--    normaliza ([],[2,5,3])   ==  ([3,5,2],[])
--    normaliza ([4],[2,5,3])  ==  ([4],[2,5,3])
normaliza :: ([a],[a]) -> ([a],[a])
normaliza ([], ys) = (reverse ys, [])
normaliza p        = p

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    λ> primero (inserta 5 (inserta 2 (inserta 3 vacia)))
--    3
primero  :: Cola a -> a
primero (C (x:_,_)) = x
primero _          = error "primero: cola vacia"

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    λ> resto (inserta 5 (inserta 2 (inserta 3 vacia)))
--    2 | 5
resto  :: Cola a -> Cola a
resto (C ([],[]))   = error "resto: cola vacia"
resto (C (_:xs,ys)) = C (normaliza (xs,ys))
resto (C ([],_:_))  = error "Imposible"

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia (inserta 5 (inserta 2 (inserta 3 vacia))) == False
--    esVacia vacia == True
esVacia :: Cola a -> Bool
esVacia (C (xs,_)) = null xs

-- (valida c) se verifica si la cola c es válida; es decir, si
-- su primer elemento es vacío entonces también lo es el segundo. Por
-- ejemplo,
--    valida (C ([2],[5]))  ==  True
--    valida (C ([2],[]))   ==  True
--    valida (C ([],[5]))   ==  False
valida :: Cola a -> Bool
valida (C (xs,ys)) = not (null xs) || null ys

-- ---------------------------------------------------------------------
-- Igualdad de colas                                                  --
-- ---------------------------------------------------------------------

-- (elementos c) es la lista de los elementos de la cola c en el orden de
-- la cola. Por ejemplo,
--    λ> elementos (inserta 5 (inserta 2 (inserta 3 vacia)))
--    [3,2,5]
elementos :: Cola a -> [a]
elementos (C (xs,ys)) = xs ++ reverse ys

-- (igualColas c1 c2) se verifica si las colas c1 y c2 son iguales. Por
-- ejemplo,
--    igualColas (C ([3,2],[5,4,7])) (C ([3],[5,4,7,2]))   ==  True
--    igualColas (C ([3,2],[5,4,7])) (C ([],[5,4,7,2,3]))  ==  False
igualColas :: Eq a => Cola a -> Cola a -> Bool
igualColas c1 c2 =
  valida c1 &&
  valida c2 &&
  elementos c1 == elementos c2

instance Eq a => Eq (Cola a) where
  (==) = igualColas

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
