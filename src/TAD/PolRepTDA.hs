-- PolRepTDA.hs
-- Implementación de los polinomios mediante tipos de datos algebraicos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-abril-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.PolRepTDA
  ( Polinomio,
    polCero,   -- Polinomio a
    esPolCero, -- Polinomio a -> Bool
    consPol,   -- (Num a, Eq a)) => Int -> a -> Polinomio a -> Polinomio a
    grado,     -- Polinomio a -> Int
    coefLider, -- Num t => Polinomio t -> t
    restoPol   -- Polinomio t -> Polinomio t
  ) where

import Test.QuickCheck

-- Representamos un polinomio mediante los constructores ConsPol y
-- PolCero. Por ejemplo, el polinomio
--    6x^4 -5x^2 + 4x -7
-- se representa por
--    ConsPol 4 6 (ConsPol 2 (-5) (ConsPol 1 4 (ConsPol 0 (-7) PolCero)))

-- Polinomio como tipo de dato algebra
data Polinomio a = PolCero
                 | ConsPol Int a (Polinomio a)
  deriving Eq

-- (escribePol p) es la cadena correspondiente al polinomio p. Por
-- ejemplo,
--    λ> escribePol (consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero)))
--    "3*x^4 + -5*x^2 + 3"
escribePol :: (Num a, Show a, Eq a) => Polinomio a -> String
escribePol PolCero               = "0"
escribePol (ConsPol 0 b PolCero) = show b
escribePol (ConsPol 0 b p)       = concat [show b, " + ", escribePol p]
escribePol (ConsPol 1 b PolCero) = show b ++ "*x"
escribePol (ConsPol 1 b p)       = concat [show b, "*x + ", escribePol p]
escribePol (ConsPol n 1 PolCero) = "x^" ++ show n
escribePol (ConsPol n b PolCero) = concat [show b, "*x^", show n]
escribePol (ConsPol n 1 p)       = concat ["x^", show n, " + ", escribePol p]
escribePol (ConsPol n b p)       = concat [show b, "*x^", show n, " + ", escribePol p]

-- Procedimiento de escritura de polinomios.
instance (Num a, Show a, Eq a) => Show (Polinomio a) where
  show = escribePol

-- Ejemplos de polinomios con coeficientes enteros:
ejPol1, ejPol2, ejPol3 :: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)

-- Comprobación de escritura:
--    > ejPol1
--    3*x^4 + -5*x^2 + 3
--    > ejPol2
--    x^5 + 5*x^2 + 4*x
--    > ejPol3
--    6*x^4 + 2*x

-- polCero es el polinomio cero. Por ejemplo,
--    > polCero
--    0
polCero :: Polinomio a
polCero = PolCero

-- (esPolCero p) se verifica si p es el polinomio cero. Por ejemplo,
--    esPolCero polCero  ==  True
--    esPolCero ejPol1   ==  False
esPolCero :: Polinomio a -> Bool
esPolCero PolCero = True
esPolCero _       = False

-- (consPol n b p) es el polinomio bx^n+p. Por ejemplo,
--    ejPol2               ==  x^5 + 5*x^2 + 4*x
--    consPol 3 0 ejPol2   ==  x^5 + 5*x^2 + 4*x
--    consPol 3 2 polCero  ==  2*x^3
--    consPol 6 7 ejPol2   ==  7*x^6 + x^5 + 5*x^2 + 4*x
--    consPol 4 7 ejPol2   ==  x^5 + 7*x^4 + 5*x^2 + 4*x
--    consPol 5 7 ejPol2   ==  8*x^5 + 5*x^2 + 4*x
consPol :: (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a
consPol _ 0 p = p
consPol n b PolCero = ConsPol n b PolCero
consPol n b (ConsPol m c p)
  | n > m      = ConsPol n b (ConsPol m c p)
  | n < m      = ConsPol m c (consPol n b p)
  | b+c == 0   = p
  | otherwise  = ConsPol n (b+c) p

-- (grado p) es el grado del polinomio p. Por ejemplo,
--    ejPol3        ==  6*x^4 + 2*x
--    grado ejPol3  ==  4
grado :: Polinomio a -> Int
grado PolCero         = 0
grado (ConsPol n _ _) = n

-- (coefLider p) es el coeficiente líder del polinomio p. Por ejemplo,
--    ejPol3            ==  6*x^4 + 2*x
--    coefLider ejPol3  ==  6
coefLider :: Num t => Polinomio t -> t
coefLider PolCero         = 0
coefLider (ConsPol _ b _) = b

-- (restoPol p) es el resto del polinomio p. Por ejemplo,
--    ejPol3           ==  6*x^4 + 2*x
--    restoPol ejPol3  ==  2*x
--    ejPol2           ==  x^5 + 5*x^2 + 4*x
--    restoPol ejPol2  ==  5*x^2 + 4*x
restoPol :: Polinomio t -> Polinomio t
restoPol PolCero         = PolCero
restoPol (ConsPol _ _ p) = p

-- Generador de polinomios                                          --
-- =======================

-- genPolinomio es un generador de polinomios. Por ejemplo,
--    λ> sample (genPol 1)
--    7*x^9 + 9*x^8 + 10*x^7 + -14*x^5 + -15*x^2 + -10
--    -4*x^8 + 2*x
--    -8*x^9 + 4*x^8 + 2*x^6 + 4*x^5 + -6*x^4 + 5*x^2 + -8*x
--    -9*x^9 + x^5 + -7
--    8*x^10 + -9*x^7 + 7*x^6 + 9*x^5 + 10*x^3 + -1*x^2
--    7*x^10 + 5*x^9 + -5
--    -8*x^10 + -7
--    -5*x
--    5*x^10 + 4*x^4 + -3
--    3*x^3 + -4
--    10*x
genPol :: (Num a, Arbitrary a, Eq a) => Int -> Gen (Polinomio a)
genPol 0 = return polCero
genPol _ = do
  n <- choose (0,10)
  b <- arbitrary
  p <- genPol (div n 2)
  return (consPol n b p)

instance (Num a, Arbitrary a, Eq a) => Arbitrary (Polinomio a) where
  arbitrary = sized genPol

-- Propiedades de los polinomios
-- =============================

-- polCero es el polinomio cero.
prop_polCero_es_cero :: Bool
prop_polCero_es_cero =
  esPolCero polCero

-- Si n es mayor que el grado de p y b no es cero, entonces
-- (consPol n b p) es un polinomio distinto del cero.
prop_consPol_no_cero :: Int -> Int -> Polinomio Int -> Property
prop_consPol_no_cero n b p =
  n > grado p && b /= 0  ==>
  not (esPolCero (consPol n b p))

-- (consPol (grado p) (coefLider p) (restoPol p)) es igual a p.
prop_consPol :: Polinomio Int -> Bool
prop_consPol p =
  consPol (grado p) (coefLider p) (restoPol p) == p

-- Si n es mayor que el grado de p y b no es cero, entonces
-- el grado de (consPol n b p) es n.
prop_grado :: Int -> Int -> Polinomio Int -> Property
prop_grado n b p =
  n > grado p && b /= 0 ==>
  grado (consPol n b p) == n

-- Si n es mayor que el grado de p y b no es cero, entonces
-- el coeficiente líder de (consPol n b p) es b.
prop_coefLider :: Int -> Int -> Polinomio Int -> Property
prop_coefLider n b p =
  n > grado p && b /= 0 ==>
  coefLider (consPol n b p) == b

-- Si n es mayor que el grado de p y b no es cero, entonces
-- el resto de (consPol n b p) es p.
prop_restoPol :: Int -> Int -> Polinomio Int -> Property
prop_restoPol n b p =
  n > grado p && b /= 0 ==>
  restoPol (consPol n b p) == p

-- Verificación
-- ============

return []

verificaPol :: IO Bool
verificaPol = $quickCheckAll

-- La verificación es
--    λ> verificaPol
--    === prop_polCero_es_cero from PolPropiedades.hs:53 ===
--    +++ OK, passed 1 test.
--
--    === prop_consPol_no_cero from PolPropiedades.hs:63 ===
--    +++ OK, passed 100 tests; 251 discarded.
--
--    === prop_consPol from PolPropiedades.hs:73 ===
--    +++ OK, passed 100 tests.
--
--    === prop_grado from PolPropiedades.hs:83 ===
--    +++ OK, passed 100 tests; 321 discarded.
--
--    === prop_coefLider from PolPropiedades.hs:94 ===
--    +++ OK, passed 100 tests; 340 discarded.
--
--    === prop_restoPol from PolPropiedades.hs:105 ===
--    +++ OK, passed 100 tests; 268 discarded.
--
--    True
