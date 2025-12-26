-- Triangulos_de_Heron.hs
-- Triángulos de Herón.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-Enero-2015 (actualizado 26-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ------------------------------------------------------------------------
-- Un triángulo de Herón es un triángulo tal que sus lados y su área son
-- números enteros. Su nombre se debe al matemático griego Herón de
-- Alejandría que descubrió la fórmula para calcular el área de un
-- triángulo a partir de sus lados.
--
-- La fórmula dice que el área de un triángulo cuyos lados miden a, b y
-- c es la raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el semiperímetro
-- del triángulo; es decir, s es (a+b+c)/2.
--
-- Un ejemplo de triángulo de Herón es el triángulo de lados 3, 4 y 5
-- cuya área es 6. Se puede observar que cualquier triángulo cuyos lados
-- sean múltiplos de 3, 4 y 5 también es de Herón; por ejemplo, el de
-- lados 6, 8 y 10 también lo es
--
-- Se dice que un triángulo de Herón es primitivo si el máximo común
-- divisor de sus lados es 1. Por ejemplo, el de lados 3, 4 y 5 es
-- primitivo; pero el de lados 6, 8 y 10 no lo es.
--
-- Definir la sucesión
--    triangulosHeronPrimitivos :: [(Int,Int,Int)]
-- tal que sus elementos son los triángulos de Herón primitivos
-- ordenados por su perímetro. Por ejemplo,
--    λ> take 7 triangulosHeronPrimitivos
--    [(3,4,5),(5,5,6),(5,5,8),(5,12,13),(4,13,15),(10,13,13),(9,10,17)]
--    λ> triangulosHeronPrimitivos !! 1000
--    (212,225,247)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Triangulos_de_Heron where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

triangulosHeronPrimitivos1 :: [(Int,Int,Int)]
triangulosHeronPrimitivos1 =
  [(a,b,c)
  | p <- [3..]
  , c <- [1..p]
  , b <- [1..c]
  , let a = p-b-c
  , 0 < a, a <= b
  , esTrianguloHeronPrimitivo a b c
  ]

-- (esTrianguloHeronPrimitivo a b c) se verifica si a, b y c
-- (con a <= b <= c) son los lados de un triángulo de Herón
-- primitivo. Por ejemplo,
--    esTrianguloHeronPrimitivo 3 4  5  ==  True
--    esTrianguloHeronPrimitivo 1 1  2  ==  False
--    esTrianguloHeronPrimitivo 6 8 10  ==  False
esTrianguloHeronPrimitivo :: Int -> Int -> Int -> Bool
esTrianguloHeronPrimitivo a b c =
  esTriangulo a b c &&
  mcd a b c == 1 &&
  even p &&
  esCuadrado (s*(s-a)*(s-b)*(s-c))
  where p = a+b+c
        s = p `div` 2

-- (esTriangulo a b c) se verifica si los números a, b y c (con a <= b <= c)
-- pueden ser los lados de un triángulo (es decir, cada uno es menor que
-- la suma de los otros dos). Por ejemplo,
--    esTriangulo 3 4 5  ==  True
--    esTriangulo 1 1 2  ==  False
esTriangulo :: Int -> Int -> Int -> Bool
esTriangulo a b c = c < a+b

-- (esCuadrado n) se verifica si n es un cuadrado perfecto. Por ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Int -> Bool
esCuadrado n = x*x == n
  where x = round (sqrt (fromIntegral n))

-- (mcd a b c) es el máximo común divisor de a, b y c. Por ejemplo,
--    mcd 3 4 5   ==  1
--    mcd 6 8 10  ==  2
mcd :: Int -> Int -> Int -> Int
mcd a b c = gcd a (gcd b c)

-- 2ª solución
-- ===========

triangulosHeronPrimitivos2 :: [(Int, Int, Int)]
triangulosHeronPrimitivos2 =
    [ (a, b, c)
    | p <- [12, 14..]
    , c <- [p `quot` 3 .. p `quot` 2]
    , b <- [(p - c + 1) `quot` 2 .. c]
    , let a = p - b - c
    , a + b > c
    , mcd a b c == 1
    , esAreaEntera a b c
    ]

-- (esAreaEntera a b c) se verifica si el área del triángulo de lados a,
-- b y c es entera.
esAreaEntera :: Int -> Int -> Int -> Bool
esAreaEntera a b c = esCuadrado2 areaCuadrada
  where
    s = (a + b + c) `quot` 2
    areaCuadrada = s * (s - a) * (s - b) * (s - c)

esCuadrado2 :: Int -> Bool
esCuadrado2 n =
  (n `rem` 16) `elem` [0, 1, 4, 9] &&
  m * m == n
  where
    m   = round (sqrt (fromIntegral n))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [(Int, Int, Int)] -> Spec
specG triangulosHeronPrimitivos = do
  it "e1" $
    take 7 triangulosHeronPrimitivos `shouldBe`
    [(3,4,5),(5,5,6),(5,5,8),(5,12,13),(4,13,15),(10,13,13),(9,10,17)]

spec :: Spec
spec = do
  describe "def. 1" $ specG triangulosHeronPrimitivos1
  describe "def. 2" $ specG triangulosHeronPrimitivos2

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
  triangulosHeronPrimitivos1 !! n == triangulosHeronPrimitivos2 !! n

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> triangulosHeronPrimitivos1 !! 300
--    (91,115,116)
--    (4.24 secs, 2,336,488,384 bytes)
--    λ> triangulosHeronPrimitivos2 !! 300
--    (91,115,116)
--    (0.38 secs, 219,722,240 bytes)
