-- La_funcion_de_Fibonacci_por_programacion_dinamica.hs
-- La función de Fibonacci por programación dinámica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-septiembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los primeros términos de la sucesión de Fibonacci son
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, ...
--
-- Escribir dos definiciones (una recursiva y otra con programación
-- dinámica) de la función
--    fib :: Integer -> Integer
-- tal que (fib n) es el n-ésimo término de la sucesión de Fibonacci. Por ejemplo,
--    fib 6 == 8
--
-- Comparar la eficiencia de las dos definiciones.
-- ---------------------------------------------------------------------

module La_funcion_de_Fibonacci_por_programacion_dinamica where

import Data.Array
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición (por recursión)
-- =============================

fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

-- 2ª definición (con programación dinámica)
-- =========================================

fib2 :: Integer -> Integer
fib2 n = vectorFib2 n ! n

-- (vectorFib2 n) es el vector con índices de 0 a n tal que el valor
-- de la posición i es el i-ésimo número de Finonacci. Por ejemplo,
--    λ> vectorFib2 7
--    array (0,7) [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13)]
vectorFib2 :: Integer -> Array Integer Integer
vectorFib2 n = v where
  v = array (0,n) [(i,f i) | i <- [0..n]]
  f 0 = 0
  f 1 = 1
  f m = v!(m-1) + v!(m-2)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> fib1 34
--    5702887
--    (11.82 secs, 3,504,944,704 bytes)
--    λ> fib2 34
--    5702887
--    (0.01 secs, 587,808 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    fib1 6 `shouldBe` 8
  it "e2" $
    fib2 6 `shouldBe` 8
  it "e3" $
    map fib1 [0..9] `shouldBe` map fib2 [0..9]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--
--    Finished in 0.0007 seconds
--    3 examples, 0 failures
--    (0.01 secs, 788,952 bytes)
