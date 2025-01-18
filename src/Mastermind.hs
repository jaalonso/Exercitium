-- Mastermind.hs
-- Mastermind.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-enero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El Mastermind es un juego que consiste en deducir un código
-- numérico formado por una lista de números. Cada vez que se empieza
-- una partida, el programa debe elegir un código, que será lo que el
-- jugador debe adivinar en la menor cantidad de intentos posibles. Cada
-- intento consiste en una propuesta de un código posible que propone el
-- jugador, y una respuesta del programa. Las respuestas le darán pistas
-- al jugador para que pueda deducir el código.
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mastermind where

import qualified Data.Set as S
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

mastermind1 :: [Int] -> [Int] -> (Int, Int)
mastermind1 xs ys =
  (length (aciertos xs ys), length (coincidencias xs ys))

-- (aciertos xs ys) es la lista de las posiciones de los aciertos entre
-- xs e ys. Por ejemplo,
--    aciertos [1,1,0,7] [1,0,1,7]  ==  [0,3]
aciertos :: Eq a => [a] -> [a] -> [Int]
aciertos xs ys =
  [n | (n,x,y) <- zip3 [0..] xs ys, x == y]

-- (coincidencia xs ys) es la lista de las posiciones de las
-- coincidencias entre xs e ys. Por ejemplo,
--    coincidencias [1,1,0,7] [1,0,1,7]  ==  [1,2]
coincidencias :: Eq a => [a] -> [a] -> [Int]
coincidencias xs ys =
  [n | (n,y) <- zip [0..] ys,
       y `elem` xs,
       n `notElem` aciertos xs ys]

-- 2ª solución
-- ===========

mastermind2 :: [Int] -> [Int] -> (Int, Int)
mastermind2 xs ys =
  (length aciertos2, length coincidencias2)
  where
    aciertos2, coincidencias2 :: [Int]
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

-- 4ª solución
-- ===========

mastermind4 :: [Int] -> [Int] -> (Int, Int)
mastermind4 xs ys =
  (length aciertos4, length coincidencias4)
  where
    aciertos4, coincidencias4 :: [Int]
    aciertos4      = [n | (n,x,y) <- zip3 [0..] xs ys, x == y]
    xs'            = S.fromList xs
    coincidencias4 = [n | (n,y) <- zip [0..] ys, y `S.member` xs', n `notElem` aciertos4]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int] -> (Int, Int)) -> Spec
specG mastermind = do
  it "e1" $
    mastermind [3,3] [3,2]          `shouldBe`  (1,0)
  it "e2" $
    mastermind [3,5,3] [3,2,5]      `shouldBe`  (1,1)
  it "e3" $
    mastermind [3,5,3,2] [3,2,5,3]  `shouldBe`  (1,3)
  it "e4" $
    mastermind [3,5,3,3] [3,2,5,3]  `shouldBe`  (2,1)

spec :: Spec
spec = do
  describe "def. 1" $ specG mastermind1
  describe "def. 2" $ specG mastermind2
  describe "def. 3" $ specG mastermind3
  describe "def. 4" $ specG mastermind4

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_mastermind :: [Int] -> [Int] -> Bool
prop_mastermind xs ys =
  all (== mastermind1 xs1 ys1)
      [mastermind2 xs1 ys1,
       mastermind3 xs1 ys1,
       mastermind4 xs1 ys1]
  where n   = min (length xs) (length ys)
        xs1 = take n xs
        ys1 = take n ys

-- La comprobación es
--    λ> quickCheck prop_mastermind
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mastermind [1..10^4] (map (*2) [1..10^4])
--    (0,5000)
--    (14.17 secs, 11,209,750,408 bytes)
--    λ> mastermind2 [1..10^4] (map (*2) [1..10^4])
--    (0,5000)
--    (0.83 secs, 8,190,200 bytes)
--    λ> mastermind3 [1..10^4] (map (*2) [1..10^4])
--    (0,5000)
--    (0.61 secs, 7,339,232 bytes)
--    λ> mastermind4 [1..10^4] (map (*2) [1..10^4])
--    (0,5000)
--    (0.03 secs, 8,910,128 bytes)
