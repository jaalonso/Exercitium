-- PolRepDensa.hs
-- Implementación de polinomios mediante listas densas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-abril-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TAD.PolRepDensa
  ( Polinomio,
    polCero,   -- Polinomio a
    esPolCero, -- Polinomio a -> Bool
    consPol,   -- (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a
    grado,     -- Polinomio a -> Int
    coefLider, -- Num a => Polinomio a -> a
    restoPol   -- (Num a, Eq a) => Polinomio a -> Polinomio a
  ) where

import Test.QuickCheck

-- Representaremos un polinomio por la lista de sus coeficientes ordenados
-- en orden decreciente según el grado. Por ejemplo, el polinomio
--    6x^4 -5x^2 + 4x -7
-- se representa por
--    [6,0,-2,4,-7].

newtype Polinomio a = Pol [a]
  deriving Eq

-- (escribePol p) es la cadena correspondiente al polinomio p. Por
-- ejemplo,
--    λ> escribePol (consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero)))
--    "3*x^4 + -5*x^2 + 3"
escribePol :: (Num a, Show a, Eq a) => Polinomio a -> String
escribePol pol
  | esPolCero pol         = "0"
  | n == 0 && esPolCero p = show a
  | n == 0                = concat [show a, " + ", escribePol p]
  | n == 1 && esPolCero p = show a ++ "*x"
  | n == 1                = concat [show a, "*x + ", escribePol p]
  | a == 1 && esPolCero p = "x^" ++ show n
  | esPolCero p           = concat [show a, "*x^", show n]
  | a == 1                = concat ["x^", show n, " + ", escribePol p]
  | otherwise             = concat [show a, "*x^", show n, " + ", escribePol p]
  where n = grado pol
        a = coefLider pol
        p = restoPol pol

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
--    λ> polCero
--    0
polCero :: Polinomio a
polCero = Pol []

-- (esPolCero p) se verifica si p es el polinomio cero. Por ejemplo,
--    esPolCero polCero  ==  True
--    esPolCero ejPol1   ==  False
esPolCero :: Polinomio a -> Bool
esPolCero (Pol []) = True
esPolCero _        = False

-- (consPol n b p) es el polinomio bx^n+p. Por ejemplo,
--    ejPol2               ==  x^5 + 5*x^2 + 4*x
--    consPol 3 0 ejPol2   ==  x^5 + 5*x^2 + 4*x
--    consPol 3 2 polCero  ==  2*x^3
--    consPol 6 7 ejPol2   ==  7*x^6 + x^5 + 5*x^2 + 4*x
--    consPol 4 7 ejPol2   ==  x^5 + 7*x^4 + 5*x^2 + 4*x
--    consPol 5 7 ejPol2   ==  8*x^5 + 5*x^2 + 4*x
consPol :: (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a
consPol _ 0 p = p
consPol n b p@(Pol xs)
    | esPolCero p = Pol (b : replicate n 0)
    | n > m       = Pol (b : replicate (n-m-1) 0 ++ xs)
    | n < m       = consPol m c (consPol n b (restoPol p))
    | b+c == 0    = Pol (dropWhile (==0) (tail xs))
    | otherwise   = Pol ((b+c):tail xs)
    where
      c = coefLider p
      m = grado p

-- (grado p) es el grado del polinomio p. Por ejemplo,
--    ejPol3        ==  6*x^4 + 2*x
--    grado ejPol3  ==  4
grado:: Polinomio a -> Int
grado (Pol []) = 0
grado (Pol xs) = length xs - 1

-- (coefLider p) es el coeficiente líder del polinomio p. Por ejemplo,
--    ejPol3            ==  6*x^4 + 2*x
--    coefLider ejPol3  ==  6
coefLider :: Num t => Polinomio t -> t
coefLider (Pol [])    = 0
coefLider (Pol (a:_)) = a

-- (restoPol p) es el resto del polinomio p. Por ejemplo,
--    ejPol3           ==  6*x^4 + 2*x
--    restoPol ejPol3  ==  2*x
--    ejPol2           ==  x^5 + 5*x^2 + 4*x
--    restoPol ejPol2  ==  5*x^2 + 4*x
restoPol :: (Num t, Eq t) => Polinomio t -> Polinomio t
restoPol (Pol [])     = polCero
restoPol (Pol [_])    = polCero
restoPol (Pol (_:b:as))
  | b == 0    = Pol (dropWhile (==0) as)
  | otherwise = Pol (b:as)

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
--    === prop_polCero_es_cero from /home/jalonso/alonso/estudio/Exercitium/Exercitium/src/TAD/PolRepDensa.hs:157 ===
--    +++ OK, passed 1 test.
--
--    === prop_consPol_no_cero from /home/jalonso/alonso/estudio/Exercitium/Exercitium/src/TAD/PolRepDensa.hs:163 ===
--    +++ OK, passed 100 tests; 274 discarded.
--
--    === prop_consPol from /home/jalonso/alonso/estudio/Exercitium/Exercitium/src/TAD/PolRepDensa.hs:169 ===
--    +++ OK, passed 100 tests.
--
--    === prop_grado from /home/jalonso/alonso/estudio/Exercitium/Exercitium/src/TAD/PolRepDensa.hs:175 ===
--    +++ OK, passed 100 tests; 297 discarded.
--
--    === prop_coefLider from /home/jalonso/alonso/estudio/Exercitium/Exercitium/src/TAD/PolRepDensa.hs:182 ===
--    +++ OK, passed 100 tests; 248 discarded.
--
--    === prop_restoPol from /home/jalonso/alonso/estudio/Exercitium/Exercitium/src/TAD/PolRepDensa.hs:189 ===
--    +++ OK, passed 100 tests; 322 discarded.
--
--    True
