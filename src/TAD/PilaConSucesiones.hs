-- PilaConSucesiones.hs
-- Implementación de las pilas mediante sucesiones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-enero-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.PilaConSucesiones
  (Pila,
   vacia,      -- Pila a
   apila,      -- a -> Pila a -> Pila a
   cima,       -- Pila a -> a
   desapila,   -- Pila a -> Pila a
   esVacia,    -- Pila a -> Bool
   escribePila -- Show a => Pila a -> String
  ) where

import Data.Sequence as S
import Test.QuickCheck

-- Representación de las pilas mediante sucesiones.
newtype Pila a = P (Seq a)
  deriving Eq

-- (escribePila p) es la cadena correspondiente a la pila p. Por
-- ejemplo,
--    escribePila (apila 5 (apila 2 (apila 3 vacia))) == "5 | 2 | 3"
escribePila :: Show a => Pila a -> String
escribePila (P xs) = case viewl xs of
    EmptyL   -> "-"
    x :< xs' -> case viewl xs' of
        EmptyL -> show x
        _      -> show x ++ " | " ++ escribePila (P xs')

-- Procedimiento de escritura de pilas.
instance Show a => Show (Pila a) where
  show = escribePila

-- Ejemplo de pila:
--    λ> apila 1 (apila 2 (apila 3 vacia))
--    1 | 2 | 3

-- vacia es la pila vacía. Por ejemplo,
--    λ> vacia
--    -
vacia   :: Pila a
vacia = P empty

-- (apila x p) es la pila obtenida añadiendo x encima de la pila p. Por
-- ejemplo,
--    λ> apila 4 (apila 3 (apila 2 (apila 5 vacia)))
--    5 | 2 | 3 | 4
apila :: a -> Pila a -> Pila a
apila x (P xs) = P (x <| xs)

-- (cima p) es la cima de la pila p. Por ejemplo,
--    λ> cima (apila 4 (apila 3 (apila 2 (apila 5 vacia))))
--    4
cima :: Pila a -> a
cima (P xs) = case viewl xs of
  EmptyL -> error "cima de la pila vacia"
  x :< _ -> x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo,
--    λ> desapila (apila 4 (apila 3 (apila 2 (apila 5 vacia))))
--    3 | 2 | 5
desapila :: Pila a -> Pila a
desapila (P xs) = case viewl xs of
  EmptyL   -> error "desapila la pila vacia"
  _ :< xs' -> P xs'

-- (esVacia p) se verifica si p es la pila vacía. Por ejemplo,
--    esVacia (apila 1 (apila 2 (apila 3 vacia))) ==  False
--    esVacia vacia                               ==  True
esVacia :: Pila a -> Bool
esVacia (P xs) = S.null xs

-- Generador de pilas                                          --
-- ==================

-- genPila es un generador de pilas. Por ejemplo,
--    λ> sample genPila
--    -
--    -2
--    -
--    4 | -1 | 5 | 4 | -4 | 3
--    -8 | 2
--    4
--    5 | 7 | 10 | 6 | -4 | 11 | -1 | 0 | 7 | -3
--    -1 | -10
--    2 | -3 | -4 | 15 | -15 | 1 | -10 | -2 | -4 | 6 | -13 | 16 | -8 | 3 | 7
--    6
--    1 | -6 | -19 | 15 | -5 | -4 | -6 | -12 | -13 | 11 | 19 | -18 | -14 | -13 | -15
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do
  xs <- listOf arbitrary
  return (foldr apila vacia xs)

-- El tipo pila es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
  arbitrary = genPila

-- Propiedades
-- ===========

-- Las propiedades son
prop_pilas :: Int -> Pila Int -> Bool
prop_pilas x p =
  cima (apila x p) == x &&
  desapila (apila x p) == p &&
  esVacia vacia &&
  not (esVacia (apila x p))

-- La comprobación e:
--    λ> quickCheck prop_pilas
--    +++ OK, passed 100 tests.
