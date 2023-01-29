-- ConjuntoConListasNoOrdenadasSinDuplicados.hs
-- Implementación de conjuntos mediante listas no ordenadas sin duplicados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-enero-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ConjuntoConListasNoOrdenadasSinDuplicados
  (Conj,
   vacio,     -- Conj a
   inserta,   -- Ord a => a -> Conj a -> Conj a
   menor,     -- Ord a => Conj a -> a
   elimina,   -- Ord a => a -> Conj a -> Conj a
   pertenece, -- Ord a => a -> Conj a -> Bool
   esVacio    -- Conj a -> Bool
  ) where

import Data.List (intercalate, sort)
import Test.QuickCheck

-- Los conjuntos como listas no ordenadas sin repeticiones.
newtype Conj a = Cj [a]

-- (escribeConjunto c) es la cadena correspondiente al conjunto c. Por
-- ejemplo,
--    λ> escribeConjunto (Cj [])
--    "{}"
--    λ> escribeConjunto (Cj [5])
--    "{5}"
--    λ> escribeConjunto (Cj [2, 5])
--    "{2, 5}"
--    λ> escribeConjunto (Cj [5, 2])
--    "{2, 5}"
escribeConjunto :: (Show a, Ord a) => Conj a -> String
escribeConjunto (Cj xs) =
  "{" ++ intercalate ", " (map show (sort xs)) ++ "}"

-- Procedimiento de escritura de conjuntos.
instance (Show a, Ord a) => Show (Conj a) where
  show = escribeConjunto

-- vacio es el conjunto vacío. Por ejemplo,
--    λ> vacio
--    {}
vacio :: Conj a
vacio = Cj []

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
--    λ> inserta 5 vacio
--    {5}
--    λ> inserta 2 (inserta 5 vacio)
--    {2, 5}
--    λ> inserta 5 (inserta 2 vacio)
--    {2, 5}
inserta :: Eq a => a -> Conj a -> Conj a
inserta x s@(Cj xs) | pertenece x s = s
                    | otherwise     = Cj (x:xs)

-- (menor c) es el menor elemento del conjunto c. Por ejemplo,
--    λ> menor (inserta 5 (inserta 2 vacio))
--    2
menor :: Ord a => Conj a -> a
menor (Cj []) = error "conjunto vacío"
menor (Cj xs) = minimum xs

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    λ> elimina 2 (inserta 5 (inserta 2 vacio))
--    {5}
elimina :: Eq a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj [y | y <- s, y /= x]

-- (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo,
--    λ> esVacio (inserta 5 (inserta 2 vacio))
--    False
--    λ> esVacio vacio
--    True
esVacio :: Conj a -> Bool
esVacio (Cj xs) = null xs

-- (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo,
--    λ> pertenece 2 (inserta 5 (inserta 2 vacio))
--    True
--    λ> pertenece 4 (inserta 5 (inserta 2 vacio))
--    False
pertenece :: Eq a => a -> Conj a -> Bool
pertenece x (Cj xs) = x `elem` xs

-- (subconjunto c1 c2) se verifica si c1 es un subconjunto de c2. Por
-- ejemplo,
--    subconjunto (Cj [1,3,2]) (Cj [3,1,2])    ==  True
--    subconjunto (Cj [1,3,4,1]) (Cj [1,3,2])  ==  False
subconjunto :: Ord a => Conj a -> Conj a -> Bool
subconjunto (Cj xs) (Cj ys) = sublista xs ys
  where sublista [] _      = True
        sublista (z:zs) vs = elem z vs && sublista zs vs

-- (igualConjunto c1 c2) se verifica si los conjuntos c1 y c2 son
-- iguales. Por ejemplo,
--    igualConjunto (Cj [3,2,1]) (Cj [1,3,2])  ==  True
--    igualConjunto (Cj [1,3,4]) (Cj [1,3,2])  ==  False
igualConjunto :: Ord a => Conj a -> Conj a -> Bool
igualConjunto c c' =
  subconjunto c c' && subconjunto c' c

--- Los conjuntos son comparables por igualdad.
instance Ord a => Eq (Conj a) where
  (==) = igualConjunto

-- Generador de conjuntos                                          --
-- ======================

-- genConjunto es un generador de conjuntos. Por ejemplo,
--    λ> sample (genConjunto :: Gen (Conj Int))
--    {}
--    {-1, 0}
--    {-4, 1, 2}
--    {-3, 0, 2, 3, 4}
--    {-7}
--    {-10, -7, -5, -2, -1, 2, 5, 8}
--    {5, 7, 8, 10}
--    {-9, -6, -3, 8}
--    {-8, -6, -5, -1, 7, 9, 14}
--    {-18, -15, -14, -13, -3, -2, 1, 2, 4, 8, 12, 17}
--    {-17, -16, -13, -12, -11, -9, -6, -3, 0, 1, 3, 5, 6, 7, 16, 18}
genConjunto :: (Arbitrary a, Ord a) => Gen (Conj a)
genConjunto = do
  xs <- listOf arbitrary
  return (foldr inserta vacio xs)

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
