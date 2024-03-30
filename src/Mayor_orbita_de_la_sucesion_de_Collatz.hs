-- Mayor_orbita_de_la_sucesion_de_Collatz.hs
-- Mayor órbita de la sucesión de Collatz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-marzo-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se considera la siguiente operación, aplicable a cualquier número
-- entero positivo:
--    * Si el número es par, se divide entre 2.
--    * Si el número es impar, se multiplica por 3 y se suma 1.
--
-- Dado un número cualquiera, podemos calcular su órbita; es decir,
-- las imágenes sucesivas al iterar la función. Por ejemplo, la órbita
-- de 13 es
--    13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1,...
--
-- Si observamos este ejemplo, la órbita de 13 es periódica, es decir,
-- se repite indefinidamente a partir de un momento dado). La conjetura
-- de Collatz dice que siempre alcanzaremos el 1 para cualquier número
-- con el que comencemos. Ejemplos:
--    * Empezando en n = 6 se obtiene 6, 3, 10, 5, 16, 8, 4, 2, 1.
--    * Empezando en n = 11 se obtiene: 11, 34, 17, 52, 26, 13, 40, 20,
--      10, 5, 16, 8, 4, 2, 1.
--    * Empezando en n = 27, la sucesión tiene 112 pasos, llegando hasta
--      9232 antes de descender a 1:  27, 82, 41, 124, 62, 31, 94, 47,
--      142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274,
--      137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263,
--      790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502,
--      251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958,
--      479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644,
--      1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308,
--      1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122,
--      61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5,
--      16, 8, 4, 2, 1.
--
-- Definir la función
--    mayoresGeneradores :: Integer -> [Integer]
-- tal que (mayoresGeneradores n) es la lista de los números menores o
-- iguales que n cuyas órbitas de Collatz son las de mayor longitud. Por
-- ejemplo,
--    mayoresGeneradores 20      ==  [18,19]
--    mayoresGeneradores (10^6)  ==  [837799]
-- ---------------------------------------------------------------------

module Mayor_orbita_de_la_sucesion_de_Collatz where

import qualified Data.MemoCombinators as Memo (integral)
import Data.List (genericLength, genericTake)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

mayoresGeneradores1 :: Integer -> [Integer]
mayoresGeneradores1 n =
  [x | (x,y) <- ps, y == m]
  where ps = genericTake n longitudesOrbitas
        m  = maximum (map snd ps)

-- longitudesOrbita es la lista de los números junto a las longitudes de
-- las órbitas de Collatz que generan. Por ejemplo,
--    λ> take 10 longitudesOrbitas
--    [(1,1),(2,2),(3,8),(4,3),(5,6),(6,9),(7,17),(8,4),(9,20),(10,7)]
longitudesOrbitas :: [(Integer, Integer)]
longitudesOrbitas =
  [(n, genericLength (collatz n)) | n <- [1..]]

-- (siguiente n) es el siguiente de n en la sucesión de Collatz. Por
-- ejemplo,
--    siguiente 13  ==  40
--    siguiente 40  ==  20
siguiente :: Integer -> Integer
siguiente n | even n    = n `div` 2
            | otherwise = 3*n+1

-- (collatz1 n) es la órbita de Collatz de n hasta alcanzar el
-- 1. Por ejemplo,
--    collatz 13  ==  [13,40,20,10,5,16,8,4,2,1]

-- 1ª definición de collatz
collatz1 :: Integer -> [Integer]
collatz1 1 = [1]
collatz1 n = n : collatz1 (siguiente n)

-- 2ª definición de collatz
collatz2 :: Integer -> [Integer]
collatz2 n = takeWhile (/=1) (iterate siguiente n) ++ [1]

-- Usaremos la 2ª definición de collatz
collatz :: Integer -> [Integer]
collatz = collatz2

-- 2ª solución
-- ===========

mayoresGeneradores2 :: Integer -> [Integer]
mayoresGeneradores2 n =
  [x | (x,y) <- ps, y == m]
  where ps = [(x, longitudOrbita x) | x <- [1..n]]
        m  = maximum (map snd ps)

-- (longitudOrbita x) es la longitud de la órbita de x. Por ejemplo,
--    longitudOrbita 13  ==  10
longitudOrbita :: Integer -> Integer
longitudOrbita 1 = 1
longitudOrbita x = 1 + longitudOrbita (siguiente x)

-- 3ª solución
-- ===========

mayoresGeneradores3 :: Integer -> [Integer]
mayoresGeneradores3 n =
  [x | (x,y) <- ps, y == m]
  where ps = [(x, longitudOrbita2 x) | x <- [1..n]]
        m  = maximum (map snd ps)

longitudOrbita2 :: Integer -> Integer
longitudOrbita2 = Memo.integral longitudOrbita2'
  where
    longitudOrbita2' 1 = 1
    longitudOrbita2' x = 1 + longitudOrbita2 (siguiente x)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG mayoresGeneradores = do
  it "e1" $
    mayoresGeneradores 20 `shouldBe` [18,19]

spec :: Spec
spec = do
  describe "def. 1" $ specG mayoresGeneradores1
  describe "def. 2" $ specG mayoresGeneradores2
  describe "def. 3" $ specG mayoresGeneradores3

-- La verificación es
--    λ> verifica
--
--    3 examples, 0 failures

-- Equivalencia de definiciones
-- ============================

-- La propiedad es
prop_mayoresGeneradores :: Positive Integer -> Bool
prop_mayoresGeneradores (Positive n) =
  all (== mayoresGeneradores1 n)
      [mayoresGeneradores2 n,
       mayoresGeneradores3 n]

-- La comprobación es
--    λ> quickCheck prop_mayoresGeneradores
--    +++ OK, passed 100 tests.

-- Comprobación de eficiencia
-- ==========================

-- La comprobación es
--    λ> mayoresGeneradores (10^5)
--    [77031]
--    (5.43 secs, 6,232,320,064 bytes)
--    λ> mayoresGeneradores2 (10^5)
--    [77031]
--    (7.68 secs, 5,238,991,616 bytes)
--    λ> mayoresGeneradores3 (10^5)
--    [77031]
--    (0.88 secs, 571,788,736 bytes)
