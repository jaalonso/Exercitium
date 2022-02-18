-- Mastermind.hs
-- Mastermind.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El Mastermind es un juego que consiste en deducir un código
-- numérico formado por una lista de números distintos. Cada vez que se
-- empieza una partida, el programa debe elegir un código, que será lo
-- que el jugador debe adivinar en la menor cantidad de intentos
-- posibles. Cada intento consiste en una propuesta de un código posible
-- que propone el jugador, y una respuesta del programa. Las respuestas
-- le darán pistas al jugador para que pueda deducir el código.
--
-- Estas pistas indican lo cerca que estuvo el número propuesto de la
-- solución a través de dos valores: la cantidad de aciertos es la
-- cantidad de dígitos que propuso el jugador que también están en el
-- código en la misma posición. La cantidad de coincidencias es la
-- cantidad de dígitos que propuso el jugador que también están en el
-- código pero en una posición distinta.
--
-- Por ejemplo, si el código que eligió el programa es el [2,6,0,7] y
-- el jugador propone el [1,4,0,6], el programa le debe responder un
-- acierto (el 0, que está en el código original en el mismo lugar, el
-- tercero), y una coincidencia (el 6, que también está en el código
-- original, pero en la segunda posición, no en el cuarto como fue
-- propuesto). Si el jugador hubiera propuesto el [3,5,9,1], habría
-- obtenido como respuesta ningún acierto y ninguna coincidencia, ya que
-- no hay números en común con el código original. Si se obtienen
-- cuatro aciertos es porque el jugador adivinó el código y ganó el
-- juego.
--
-- Definir la función
--    mastermind :: [Int] -> [Int] -> (Int,Int)
-- tal que (mastermind xs ys) es el par formado por los números de
-- aciertos y de coincidencias entre xs e ys. Por ejemplo,
--    mastermind [3,3] [3,2]          ==  (1,0)
--    mastermind [3,5,3] [3,2,5]      ==  (1,1)
--    mastermind [3,5,3,2] [3,2,5,3]  ==  (1,3)
--    mastermind [3,5,3,3] [3,2,5,3]  ==  (2,1)
--    mastermind [1..10^6] [1..10^6]  ==  (1000000,0)
-- ---------------------------------------------------------------------

module Mastermind where

import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

mastermind :: [Int] -> [Int] -> (Int, Int)
mastermind xs ys =
  (length (aciertos xs ys), length (coincidencias xs ys))

-- (aciertos xs ys) es la lista de aciertos entre xs e ys. Por ejemplo,
--    aciertos [1,1,0,7] [1,0,1,7]  ==  [(0,1),(3,7)]
aciertos :: Eq a => [a] -> [a] -> [(Int,a)]
aciertos xs ys =
  [(n,y) | (n,x,y) <- zip3 [0..] xs ys,
           x == y]

-- (coincidencia xs ys) es la lista de coincidencias entre xs e ys. Por
-- ejemplo,
--    coincidencias [1,1,0,7] [1,0,1,7]  ==  [(1,0),(2,1)]
coincidencias :: Eq a => [a] -> [a] -> [(Int,a)]
coincidencias xs ys =
  [(n,y) | (n,y) <- zip [0..] ys,
           y `elem` xs,
           (n,y) `notElem` aciertos xs ys]

-- 2ª solución
-- ===========

mastermind2 :: [Int] -> [Int] -> (Int, Int)
mastermind2 xs ys =
  (length aciertos2, length coincidencias2)
  where
    aciertos2      = [n | (n,x,y) <- zip3 [0..] xs ys, x == y]
    coincidencias2 = [n | (n,y) <- zip [0..] ys, y `elem` xs, n `notElem` aciertos2]

-- 3ª solución
-- ===========

mastermind3 :: [Int] -> [Int] -> (Int, Int)
mastermind3 xs ys = aux xs ys
  where aux (u:us) (v:vs)
          | u == v      = (a+1,b)
          | v `elem` xs = (a,b+1)
          | otherwise   = (a,b)
          where (a,b) = aux us vs
        aux _ _ = (0,0)

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_mastermind :: [Int] -> [Int] -> Bool
prop_mastermind xs ys =
  all (== mastermind xs1 ys1)
      [mastermind2 xs1 ys1,
       mastermind3 xs1 ys1]
  where n   = min (length xs) (length ys)
        xs1 = take n xs
        ys1 = take n ys

verifica_mastermind :: IO ()
verifica_mastermind = quickCheck prop_mastermind

-- La comprobación es
--    λ> verifica_mastermind
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> mastermind [1..10^4] [1..10^4]
--    (10000,0)
--    (13.33 secs, 16,413,026,904 bytes)
--    λ> mastermind2 [1..10^4] [1..10^4]
--    (10000,0)
--    (1.08 secs, 8,187,176 bytes)
--    λ> mastermind3 [1..10^4] [1..10^4]
--    (10000,0)
--    (0.03 secs, 6,437,472 bytes)
