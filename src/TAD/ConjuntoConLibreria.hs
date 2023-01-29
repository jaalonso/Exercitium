-- ConjuntoConLibreria.hs
-- Implementación de los conjuntos mediante librería.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-enero-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD.ConjuntoConLibreria
  (Conj,
   vacio,     -- Conj a
   inserta,   -- Ord a => a -> Conj a -> Conj a
   menor,     -- Ord a => Conj a -> a
   elimina,   -- Ord a => a -> Conj a -> Conj a
   pertenece, -- Ord a => a -> Conj a -> Bool
   esVacio    -- Conj a -> Bool
  ) where

import Data.Set as S (Set, empty, insert, findMin, delete, member, null,
                      fromList, toList)
import Data.List (intercalate)
import Test.QuickCheck

-- Los conjuntos como conjuntos de la librería Data.Set
newtype Conj a = Cj (Set a)
  deriving (Eq, Ord)

-- (escribeConjunto c) es la cadena correspondiente al conjunto c. Por
-- ejemplo,
--    λ> escribeConjunto (Cj (fromList []))
--    "{}"
--    λ> escribeConjunto (Cj (fromList [5]))
--    "{5}"
--    λ> escribeConjunto (Cj (fromList [2, 5]))
--    "{2, 5}"
--    λ> escribeConjunto (Cj (fromList [5, 2]))
--    "{2, 5}"
escribeConjunto :: Show a => Conj a -> String
escribeConjunto (Cj s) =
  "{" ++ intercalate ", " (map show (toList s)) ++ "}"

-- Procedimiento de escritura de conjuntos.
instance Show a => Show (Conj a) where
  show = escribeConjunto

-- vacio es el conjunto vacío. Por ejemplo,
--    λ> vacio
--    {}
vacio :: Conj a
vacio = Cj empty

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
--    λ> inserta 5 vacio
--    {5}
--    λ> inserta 2 (inserta 5 vacio)
--    {2, 5}
--    λ> inserta 5 (inserta 2 vacio)
--    {2, 5}
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj s) = Cj (insert x s)

-- (menor c) es el menor elemento del conjunto c. Por ejemplo,
--    λ> menor (inserta 5 (inserta 2 vacio))
--    2
menor :: Ord a => Conj a -> a
menor (Cj s) = findMin s

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    λ> elimina 2 (inserta 5 (inserta 2 vacio))
--    {5}
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj (delete x s)

-- (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo,
--    λ> esVacio (inserta 5 (inserta 2 vacio))
--    False
--    λ> esVacio vacio
--    True
esVacio :: Conj a -> Bool
esVacio (Cj s) = S.null s

-- (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo,
--    λ> pertenece 2 (inserta 5 (inserta 2 vacio))
--    True
--    λ> pertenece 4 (inserta 5 (inserta 2 vacio))
--    False
pertenece :: Ord a => a -> Conj a -> Bool
pertenece x (Cj s) = member x s

-- Generador de conjuntos                                          --
-- ======================

-- genConjunto es un generador de conjuntos. Por ejemplo,
--    λ> sample (genConjunto :: Gen (Conj Int))
--    {}
--    {-2, 0}
--    {-1, 3}
--    {-3, 2}
--    {-5, -4, -3, 2, 4, 6, 7}
--    {-4, 4}
--    {-9, -6, -3, 1, 5, 11, 12}
--    {-10, -8, -7, -3, 1, 2, 8, 9, 10, 13}
--    {-13, -8, -7, -6, -1, 0, 1, 6, 7, 9, 11, 14, 16}
--    {-15, -12, -9, 1, 2, 9, 13, 15, 16, 18}
--    {-16}
genConjunto :: (Arbitrary a, Ord a) => Gen (Conj a)
genConjunto = do
  xs <- listOf arbitrary
  return (Cj (fromList xs))

-- Los conjuntos son concreciones de los arbitrarios.
instance (Arbitrary a, Ord a) => Arbitrary (Conj a) where
  arbitrary = genConjunto

-- Propiedades de los conjuntos                                        --
-- ============================

prop_conjuntos :: Int -> Int -> Conj Int -> Bool
prop_conjuntos x y c =
  inserta x (inserta x c) == inserta x c &&
  inserta x (inserta y c) == inserta y (inserta x c) &&
  not (pertenece x vacio) &&
  pertenece y (inserta x c) == (x == y) || pertenece y c &&
  elimina x vacio == vacio &&
  elimina x (inserta y c) == (if x == y
                              then elimina x c
                              else inserta y (elimina x c)) &&
  esVacio (vacio :: Conj Int) &&
  not (esVacio (inserta x c))

-- Comprobación
--    λ> quickCheck prop_conjuntos
--    +++ OK, passed 100 tests.
