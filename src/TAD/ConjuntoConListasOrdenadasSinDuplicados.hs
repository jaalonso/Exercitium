-- ConjuntoConListasOrdenadasSinDuplicados.hs
-- Implementación de los conjuntos mediante listas ordenadas sin duplicados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-enero-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.ConjuntoConListasOrdenadasSinDuplicados
  (Conj,
   vacio,     -- Conj a
   inserta,   -- Ord a => a -> Conj a -> Conj a
   menor,     -- Ord a => Conj a -> a
   elimina,   -- Ord a => a -> Conj a -> Conj a
   pertenece, -- Ord a => a -> Conj a -> Bool
   esVacio    -- Conj a -> Bool
  ) where

import Data.List (intercalate)
import Test.QuickCheck

-- Los conjuntos como listas ordenadas sin repeticiones.
newtype Conj a = Cj [a]
  deriving Eq

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
escribeConjunto :: Show a => Conj a -> String
escribeConjunto (Cj xs) =
  "{" ++ intercalate ", " (map show xs) ++ "}"

-- Procedimiento de escritura de conjuntos.
instance Show a => Show (Conj a) where
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
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj s) = Cj (agrega x s)
  where agrega z []                     = [z]
        agrega z s'@(y:ys) | z > y      = y : agrega z ys
                           | z < y      = z : s'
                           | otherwise  = s'

-- (menor c) es el menor elemento del conjunto c. Por ejemplo,
--    λ> menor (inserta 5 (inserta 2 vacio))
--    2
menor :: Ord a => Conj a -> a
menor (Cj [])    = error "conjunto vacío"
menor (Cj (x:_)) = x

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    λ> elimina 2 (inserta 5 (inserta 2 vacio))
--    {5}
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj (elimina' x s)
  where elimina' _ []                    = []
        elimina' z s'@(y:ys) | z > y     = y : elimina' z ys
                             | z < y     = s'
                             | otherwise = ys

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
pertenece :: Ord a => a -> Conj a -> Bool
pertenece x (Cj s) = x `elem` takeWhile (<= x) s

-- Generador de conjuntos                                          --
-- ======================

-- genConjunto es un generador de conjuntos. Por ejemplo,
--    λ> sample (genConjunto :: Gen (Conj Int))
--    {}
--    {1}
--    {2}
--    {}
--    {-5, -1}
--    {-9, -8, 2, 3, 10}
--    {4}
--    {-13, -7, 1, 14}
--    {-12, -10, -9, -4, 1, 2, 5, 14, 16}
--    {-18, -15, -14, -13, -10, -7, -6, -4, -1, 1, 10, 11, 12, 16}
--    {-16, -9, -6, -5, -4, -2, 3, 6, 9, 13, 17}
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
