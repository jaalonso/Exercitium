-- Ordenada_ciclicamente.hs
-- Ordenada cíclicamente.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-junio-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se dice que una sucesión x(1), ..., x(n) está ordenada cíclicamente
-- si existe un índice i tal que la sucesión
--    x(i), x(i+1), ..., x(n), x(1), ..., x(i-1)
-- está ordenada creciente de forma estricta.
--
-- Definir la función
--    ordenadaCiclicamente :: Ord a => [a] -> Maybe Int
-- tal que (ordenadaCiclicamente xs) es el índice a partir del cual está
-- ordenada, si la lista está ordenado cíclicamente y Nothing en caso
-- contrario. Por ejemplo,
--    ordenadaCiclicamente [1,2,3,4]      ==  Just 0
--    ordenadaCiclicamente [5,8,1,3]      ==  Just 2
--    ordenadaCiclicamente [4,6,7,5,1,3]  ==  Nothing
--    ordenadaCiclicamente [1,0,3,2]      ==  Nothing
--    ordenadaCiclicamente [1,2,0]        ==  Just 2
--    ordenadaCiclicamente "cdeab"        ==  Just 3
--
-- Nota: Se supone que el argumento es una lista no vacía sin elementos
-- repetidos.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module A2014.M06.Ordenada_ciclicamente where

import Data.List       (nub, sort)
import Data.Maybe      (isJust, listToMaybe)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, NonEmptyList (NonEmpty), Property,
                        arbitrary, chooseInt, collect, quickCheck)

-- 1ª solución
-- ===========

ordenadaCiclicamente1 :: Ord a => [a] -> Maybe Int
ordenadaCiclicamente1 xs = aux 0 xs
  where n = length xs
        aux i zs
          | i == n      = Nothing
          | ordenada zs = Just i
          | otherwise   = aux (i+1) (siguienteCiclo zs)

-- (ordenada xs) se verifica si la lista xs está ordenada
-- crecientemente. Por ejemplo,
--   ordenada "acd"   ==  True
--   ordenada "acdb"  ==  False
ordenada :: Ord a => [a] -> Bool
ordenada []     = True
ordenada (x:xs) = all (x <) xs && ordenada xs

-- (siguienteCiclo xs) es la lista obtenida añadiendo el primer elemento
-- de xs al final del resto de xs. Por ejemplo,
--   siguienteCiclo [3,2,5]  =>  [2,5,3]
siguienteCiclo :: [a] -> [a]
siguienteCiclo []     = []
siguienteCiclo (x:xs) = xs ++ [x]

-- 2ª solución
-- ===========

ordenadaCiclicamente2 :: Ord a => [a] -> Maybe Int
ordenadaCiclicamente2 xs =
  listToMaybe [n | n <- [0..length xs-1],
                   ordenada (drop n xs ++ take n xs)]

-- 3ª solución
-- ===========

ordenadaCiclicamente3 :: Ord a => [a] -> Maybe Int
ordenadaCiclicamente3 xs
  | ordenada (bs ++ as) = Just k
  | otherwise           = Nothing
  where (_,k)   = minimum (zip xs [0..])
        (as,bs) = splitAt k xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> Maybe Int) -> Spec
specG ordenadaCiclicamente = do
  it "e1" $
    ordenadaCiclicamente [1,2,3,4]      `shouldBe`  Just 0
  it "e2" $
    ordenadaCiclicamente [5,8,1,3]      `shouldBe`  Just 2
  it "e3" $
    ordenadaCiclicamente [4,6,7,5,1,3]  `shouldBe`  Nothing
  it "e4" $
    ordenadaCiclicamente [1,0,3,2]      `shouldBe`  Nothing
  it "e5" $
    ordenadaCiclicamente [1,2,0]        `shouldBe`  Just 2

spec :: Spec
spec = do
  describe "def. 1"  $ specG ordenadaCiclicamente1
  describe "def. 2"  $ specG ordenadaCiclicamente2
  describe "def. 3"  $ specG ordenadaCiclicamente3

-- La verificación es
--    λ> verifica
--    15 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_ordenadaCiclicamente1 :: NonEmptyList Int -> Bool
prop_ordenadaCiclicamente1 (NonEmpty xs) =
  ordenadaCiclicamente1 xs == ordenadaCiclicamente2 xs

-- La comprobación es
--    λ> quickCheck prop_ordenadaCiclicamente1
--    +++ OK, passed 100 tests.

-- La propiedad para analizar los casos de prueba
prop_ordenadaCiclicamente2 :: NonEmptyList Int -> Property
prop_ordenadaCiclicamente2 (NonEmpty xs) =
  collect (isJust (ordenadaCiclicamente1 xs)) $
  ordenadaCiclicamente1 xs == ordenadaCiclicamente2 xs

-- El análisis es
--    λ> quickCheck prop_ordenadaCiclicamente2
--    +++ OK, passed 100 tests:
--    89% False
--    11% True

-- Tipo para generar listas
newtype Lista = L [Int]
  deriving Show

-- Generador de listas.
listaArbitraria :: Gen Lista
listaArbitraria = do
  x <- arbitrary
  xs <- arbitrary
  let ys = x : xs
  k <- chooseInt (0, length ys)
  let (as,bs) = splitAt k (sort (nub ys))
  return (L (bs ++ as))

-- Lista es una subclase de Arbitrary.
instance Arbitrary Lista where
  arbitrary = listaArbitraria

-- La propiedad para analizar los casos de prueba
prop_ordenadaCiclicamente3 :: Lista -> Property
prop_ordenadaCiclicamente3 (L xs) =
  collect (isJust (ordenadaCiclicamente1 xs)) $
  ordenadaCiclicamente1 xs == ordenadaCiclicamente2 xs

-- El análisis es
--    λ> quickCheck prop_ordenadaCiclicamente3
--    +++ OK, passed 100 tests (100% True).

-- Tipo para generar
newtype Lista2 = L2 [Int]
  deriving Show

-- Generador de listas
listaArbitraria2 :: Gen Lista2
listaArbitraria2 = do
  x' <- arbitrary
  xs <- arbitrary
  let ys = x' : xs
  k <- chooseInt (0, length ys)
  let (as,bs) = splitAt k (sort (nub ys))
  n <- chooseInt (0,1)
  return (if even n
          then L2 (bs ++ as)
          else L2 ys)

-- Lista es una subclase de Arbitrary.
instance Arbitrary Lista2 where
  arbitrary = listaArbitraria2

-- La propiedad para analizar los casos de prueba
prop_ordenadaCiclicamente4 :: Lista2 -> Property
prop_ordenadaCiclicamente4 (L2 xs) =
  collect (isJust (ordenadaCiclicamente1 xs)) $
  ordenadaCiclicamente1 xs == ordenadaCiclicamente2 xs

-- El análisis es
--    λ> quickCheck prop_ordenadaCiclicamente4
--    +++ OK, passed 100 tests:
--    51% True
--    49% False

-- La propiedad es
prop_ordenadaCiclicamente :: Lista2 -> Bool
prop_ordenadaCiclicamente (L2 xs) =
  all (== ordenadaCiclicamente1 xs)
      [ordenadaCiclicamente2 xs,
       ordenadaCiclicamente3 xs]

-- La comprobación es
--    λ> quickCheck prop_ordenadaCiclicamente
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ordenadaCiclicamente1 ([100..4000] ++ [1..99])
--    Just 3901
--    (3.27 secs, 2,138,864,568 bytes)
--    λ> ordenadaCiclicamente2 ([100..4000] ++ [1..99])
--    Just 3901
--    (2.44 secs, 1,430,040,008 bytes)
--    λ> ordenadaCiclicamente3 ([100..4000] ++ [1..99])
--    Just 3901
--    (1.18 secs, 515,549,200 bytes)
