-- Integracion_por_rectangulos.hs
-- Integración por el método de los rectángulos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-noviembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La integral definida de una función f entre los límites a y b puede
-- calcularse mediante la regla del rectángulo (ver en
-- http://bit.ly/1FDhZ1z) usando la fórmula
--    h * (f(a+h/2) + f(a+h+h/2) + f(a+2h+h/2) + ... + f(a+n*h+h/2))
-- con a+n*h+h/2 <= b < a+(n+1)*h+h/2 y usando valores pequeños para h.
--
-- Definir la función
--    integral :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
-- tal que (integral a b f h) es el valor de dicha expresión. Por
-- ejemplo, el cálculo de la integral de f(x) = x^3 entre 0 y 1, con
-- paso 0.01, es
--    integral 0 1 (^3) 0.01  ==  0.24998750000000042
-- Otros ejemplos son
--    integral 0 1 (^4) 0.01                        ==  0.19998333362500048
--    integral 0 1 (\x -> 3*x^2 + 4*x^3) 0.01       ==  1.9999250000000026
--    log 2 - integral 1 2 (\x -> 1/x) 0.01         ==  3.124931644782336e-6
--    pi - 4 * integral 0 1 (\x -> 1/(x^2+1)) 0.01  ==  -8.333333331389525e-6
-- ---------------------------------------------------------------------

module Integracion_por_rectangulos where

import Test.Hspec (Spec, hspec, it, shouldBe, shouldSatisfy)

-- 1ª solución
-- ===========

integral :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral a b f h = h * suma (a+h/2) b (+h) f

-- (suma a b s f) es l valor de
--    f(a) + f(s(a)) + f(s(s(a)) + ... + f(s(...(s(a))...))
-- hasta que s(s(...(s(a))...)) > b. Por ejemplo,
--    suma 2 5 (1+) (^3)  ==  224
suma :: (Ord t, Num a) => t -> t -> (t -> t) -> (t -> a) -> a
suma a b s f = sum [f x | x <- sucesion a b s]

-- (sucesion x y s) es la lista
--    [a, s(a), s(s(a), ..., s(...(s(a))...)]
-- hasta que s(s(...(s(a))...)) > b. Por ejemplo,
--    sucesion 3 20 (+2)  ==  [3,5,7,9,11,13,15,17,19]
sucesion :: Ord a => a -> a -> (a -> a) -> [a]
sucesion a b s = takeWhile (<=b) (iterate s a)

-- 2ª solución
-- ===========

integral2 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral2 a b f h
  | a+h/2 > b = 0
  | otherwise = h * f (a+h/2) + integral2 (a+h) b f h

-- 3ª solución
-- ===========

integral3 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral3 a b f h = aux a where
  aux x | x+h/2 > b = 0
        | otherwise = h * f (x+h/2) + aux (x+h)

-- Comparación de eficiencia
--    λ> integral 0 10 (^3) 0.00001
--    2499.9999998811422
--    (4.62 secs, 1084774336 bytes)
--    λ> integral2 0 10 (^3) 0.00001
--    2499.999999881125
--    (7.90 secs, 1833360768 bytes)
--    λ> integral3 0 10 (^3) 0.00001
--    2499.999999881125
--    (7.27 secs, 1686056080 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    integral' 0 1 (^(3::Int)) 0.01 `shouldBe` 0.24998750000000042
  it "e2" $
    integral' 0 1 (^(4::Int)) 0.01 `shouldBe` 0.19998333362500048
  it "e3" $
    integral' 0 1 (\x -> 3*x^(2::Int) + 4*x^(3::Int)) 0.01 `shouldBe` 1.9999250000000026
  it "e4" $
    log 2 - integral' 1 2 (1 /) 0.01 `shouldBe` 3.124931644782336e-6
  it "e5" $
    pi - 4 * integral' 0 1 (\x -> 1/(x^(2::Int)+1)) 0.01 `shouldBe` -8.333333331389525e-6
  it "e1b" $
    integral2' 0 1 (^(3::Int)) 0.01 `shouldSatisfy` (~= 0.24998750000000042)
  it "e2b" $
    integral2' 0 1 (^(4::Int)) 0.01 `shouldSatisfy` (~= 0.19998333362500048)
  it "e3b" $
    integral2' 0 1 (\x -> 3*x^(2::Int) + 4*x^(3::Int)) 0.01 `shouldSatisfy` (~= 1.9999250000000026)
  it "e4b" $
    log 2 - integral2' 1 2 (1 /) 0.01 `shouldSatisfy` (~= 3.124931644782336e-6)
  it "e5b" $
    pi - 4 * integral2' 0 1 (\x -> 1/(x^(2::Int)+1)) 0.01 `shouldSatisfy` (~= (-8.333333331389525e-6))
  it "e1c" $
    integral3' 0 1 (^(3::Int)) 0.01 `shouldSatisfy` (~= 0.24998750000000042)
  it "e2c" $
    integral3' 0 1 (^(4::Int)) 0.01 `shouldSatisfy` (~= 0.19998333362500048)
  it "e3c" $
    integral3' 0 1 (\x -> 3*x^(2::Int) + 4*x^(3::Int)) 0.01 `shouldSatisfy` (~= 1.9999250000000026)
  it "e4c" $
    log 2 - integral3' 1 2 (1 /) 0.01 `shouldSatisfy` (~= 3.124931644782336e-6)
  it "e5c" $
    pi - 4 * integral3' 0 1 (\x -> 1/(x^(2::Int)+1)) 0.01 `shouldSatisfy` (~= (-8.333333331389525e-6))
  where
    integral', integral2', integral3' :: Double -> Double -> (Double -> Double) -> Double -> Double
    integral'  = integral
    integral2' = integral2
    integral3' = integral3
    a ~= b = abs (a - b) < 0.00001

-- La verificación es
--    λ> verifica
--
--    Finished in 0.0058 seconds
--    15 examples, 0 failures
