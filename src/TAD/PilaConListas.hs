-- PilaConListas.hs
-- Implementación de las pilas mediante listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-enero-2023
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.PilaConListas
  (Pila,
   vacia,      -- Pila a
   apila,      -- a -> Pila a -> Pila a
   cima,       -- Pila a -> a
   desapila,   -- Pila a -> Pila a
   esVacia,    -- Pila a -> Bool
   escribePila -- Show a => Pila a -> String
  ) where

import Test.QuickCheck

-- Representación de las pilas mediante listas.
newtype Pila a = P [a]
  deriving Eq

-- (escribePila p) es la cadena correspondiente a la pila p. Por
-- ejemplo,
--    escribePila (apila 5 (apila 2 (apila 3 vacia))) == "5 | 2 | 3"
escribePila :: Show a => Pila a -> String
escribePila (P [])     = "-"
escribePila (P [x])    = show x
escribePila (P (x:xs)) = show x ++ " | " ++ escribePila (P xs)

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
vacia = P []

-- (apila x p) es la pila obtenida añadiendo x encima de la pila p. Por
-- ejemplo,
--    λ> apila 4 (apila 3 (apila 2 (apila 5 vacia)))
--    4 | 3 | 2 | 5
apila :: a -> Pila a -> Pila a
apila x (P xs) = P (x:xs)

-- (cima p) es la cima de la pila p. Por ejemplo,
--    λ> cima (apila 4 (apila 3 (apila 2 (apila 5 vacia))))
--    4
cima :: Pila a -> a
cima (P [])    = error "cima de la pila vacia"
cima (P (x:_)) = x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo,
--    λ> desapila (apila 4 (apila 3 (apila 2 (apila 5 vacia))))
--    3 | 2 | 5
desapila :: Pila a -> Pila a
desapila (P [])     = error "desapila la pila vacia"
desapila (P (_:xs)) = P  xs

-- (esVacia p) se verifica si p es la pila vacía. Por ejemplo,
--    esVacia (apila 1 (apila 2 (apila 3 vacia))) ==  False
--    esVacia vacia                               ==  True
esVacia :: Pila a -> Bool
esVacia (P xs) = null xs

-- Generador de pilas                                          --
-- ==================

-- genPila es un generador de pilas. Por ejemplo,
--    λ> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
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

-- La comprobación es:
--    λ> quickCheck prop_pilas
--    +++ OK, passed 100 tests.
