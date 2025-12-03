 -- Problema_de_las_puertas.hs
-- Problema de las puertas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-Diciembre-2014 (actualizado 28-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un hotel dispone de n habitaciones y n camareros. Los camareros
-- tienen la costumbre de cambiar de estado las puertas (es decir,
-- abrir las cerradas y cerrar las abiertas). El proceso es el
-- siguiente:
-- + Inicialmente todas las puertas están cerradas.
-- + El primer camarero cambia de estado las puertas de todas las
--   habitaciones.
-- + El segundo cambia de estado de las puertas de las habitaciones
--   pares.
-- + El tercero cambia de estado todas las puertas que son
--   múltiplos de 3.
-- + El cuarto cambia de estado todas las puertas que son múltiplos
--   de 4
-- + Así hasta que ha pasado el último camarero.
-- Por ejemplo, para n = 5
--    Pase    | Puerta 1 | Puerta 2 | Puerta 3 | Puerta 4 | Puerta 5
--    --------+----------+----------+----------+----------+---------
--    Inicial | Cerrada  | Cerrada  | Cerrada  | Cerrada  | Cerrada
--    Pase 1  | Abierta  | Abierta  | Abierta  | Abierta  | Abierta
--    Pase 2  | Abierta  | Cerrada  | Abierta  | Cerrada  | Abierta
--    Pase 3  | Abierta  | Cerrada  | Cerrada  | Cerrada  | Abierta
--    Pase 4  | Abierta  | Cerrada  | Cerrada  | Abierta  | Abierta
--    Pase 5  | Abierta  | Cerrada  | Cerrada  | Abierta  | Cerrada
--
-- Los estados de las puertas se representan por el siguiente tipo de
-- datos
--    data Estado = Abierta | Cerrada deriving Show
--
-- Definir la función
--    final :: Int -> [Estado]
-- tal que (final n) es la lista de los estados de las n puertas después
-- de que hayan pasado los n camareros. Por
-- ejemplo,
--    λ> final 5
--    [Abierta,Cerrada,Cerrada,Abierta,Cerrada]
--    λ> final 7
--    [Abierta,Cerrada,Cerrada,Abierta,Cerrada,Cerrada,Cerrada]
-- ---------------------------------------------------------------------

module Problema_de_las_puertas where

import Math.NumberTheory.ArithmeticFunctions (divisors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Estado = Abierta | Cerrada
  deriving (Eq, Show)

-- 1ª solución
-- ===========

-- (cambia e) es el estdo obtenido cambiando el estado e.
cambia :: Estado -> Estado
cambia Abierta = Cerrada
cambia Cerrada = Abierta

-- (inicial n) es el estado inicial para el problema de las n
-- habitaciones. Por ejemplo,
--    inicial 5  ==  [Cerrada,Cerrada,Cerrada,Cerrada,Cerrada]
inicial :: Int -> [Estado]
inicial n = replicate n Cerrada

-- (pase k es) es la lista de los estados de las puertas después de pasar el
-- camarero k que las encuentra en los estados es. Por ejemplo,
--    λ> pase 1 (inicial 5)
--    [Abierta,Abierta,Abierta,Abierta,Abierta]
--    λ> pase 2 it
--    [Abierta,Cerrada,Abierta,Cerrada,Abierta]
--    λ> pase 3 it
--    [Abierta,Cerrada,Cerrada,Cerrada,Abierta]
--    λ> pase 4 it
--    [Abierta,Cerrada,Cerrada,Abierta,Abierta]
--    λ> pase 5 it
--    [Abierta,Cerrada,Cerrada,Abierta,Cerrada]
pase :: [Estado] -> Int -> [Estado]
pase es k = zipWith cambiaK  es [1..]
  where cambiaK e n | n `mod` k == 0 = cambia e
                    | otherwise      = e

final1 :: Int -> [Estado]
final1 n = aux [1..n] (inicial n)
  where aux []     es = es
        aux (k:ks) es = aux ks (pase es k)

-- 2ª solución
-- ===========

final2 :: Int -> [Estado]
final2 n = foldl pase (inicial n) [1..n]

-- 3ª solución
-- ===========

-- En primer lugar, vamos a determinar la lista de las posiciones
-- (comenzando a contar en 1) de las puertas que quedan abierta en el
-- problema de las n puertas.
posicionesAbiertas :: Int -> [Int]
posicionesAbiertas n =
  [x | (x,y) <- zip [1..] (final2 n), y == Abierta]

-- Al calcularlas,
--    λ> posicionesAbiertas 200
--    [1,4,9,16,25,36,49,64,81,100,121,144,169,196]
-- Se observa las que quedan abiertas son las que sus posiciones son
-- cuadrados perfectos. Usando esta observación se construye la
-- siguiente definición

final3 :: Int -> [Estado]
final3 n = map f [1..n]
  where f x | esCuadrado x = Abierta
            | otherwise    = Cerrada

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Int -> Bool
esCuadrado x = x == y * y
  where y = raiz x

-- (raiz x) es la raíz cuadrada entera de x. Por ejemplo,
--    raiz 25  ==  5
--    raiz 24  ==  4
--    raiz 26  ==  5
raiz :: Int -> Int
raiz x = truncate (sqrt (fromIntegral x))

-- 4ª solución
-- ===========

final4 :: Int -> [Estado]
final4 n = map f [1..n]
  where f x | esCuadrado2 x = Abierta
            | otherwise     = Cerrada

esCuadrado2 :: Int -> Bool
esCuadrado2 x = x == y * y
  where y = raiz2 x

raiz2 :: Int -> Int
raiz2 0 = 0
raiz2 1 = 1
raiz2 x = aux (0,x)
  where
    aux :: (Int,Int) -> Int
    aux (a,b) | d == x    = c
              | c == a    = a
              | d < x     = aux (c,b)
              | otherwise = aux (a,c)
      where c = (a+b) `div` 2
            d = c^2

-- 5ª solución
-- ===========

final5 :: Int -> [Estado]
final5 n = map f [1..n]
  where f x | esCuadrado3 x = Abierta
            | otherwise     = Cerrada

esCuadrado3 :: Int -> Bool
esCuadrado3 x =
  odd (length (divisores x))

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 30 == [1,2,3,5,6,10,15,30]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- 6ª solución
-- ===========

final6 :: Int -> [Estado]
final6 n = map f [1..n]
  where f x | esCuadrado4 x = Abierta
            | otherwise     = Cerrada

esCuadrado4 :: Int -> Bool
esCuadrado4 x =
  odd (length (divisors x))

-- 7ª solución
-- ===========

final7 :: Int -> [Estado]
final7 n = aux [1..n] [k*k | k <- [1..]]
  where aux (x:xs) (y:ys) | x == y  =  Abierta : aux xs ys
        aux (_:xs) ys               =  Cerrada : aux xs ys
        aux []     _                =  []

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Estado]) -> Spec
specG final = do
  it "e1" $
    final 5 `shouldBe` [Abierta,Cerrada,Cerrada,Abierta,Cerrada]
  it "e2" $
    final 7 `shouldBe` [Abierta,Cerrada,Cerrada,Abierta,Cerrada,Cerrada,Cerrada]

spec :: Spec
spec = do
  describe "def. 1" $ specG final1
  describe "def. 2" $ specG final2
  describe "def. 3" $ specG final3
  describe "def. 4" $ specG final4
  describe "def. 5" $ specG final5
  describe "def. 6" $ specG final6
  describe "def. 7" $ specG final7

-- La verificación es
--    λ> verifica
--    14 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Bool
prop_equivalencia (Positive n) =
  all (== final1 n)
      [final2 n,
       final3 n,
       final4 n,
       final5 n,
       final6 n,
       final7 n]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (final1 6000)
--    Cerrada
--    (2.04 secs, 7,716,402,248 bytes)
--    λ> last (final2 6000)
--    Cerrada
--    (2.05 secs, 7,715,970,128 bytes)
--    λ> last (final3 6000)
--    Cerrada
--    (0.01 secs, 1,561,840 bytes)
--    λ> last (final4 6000)
--    Cerrada
--    (0.00 secs, 1,574,688 bytes)
--    λ> last (final5 6000)
--    Cerrada
--    (0.02 secs, 2,763,864 bytes)
--    λ> last (final6 6000)
--    Cerrada
--    (0.01 secs, 1,576,872 bytes)
--    λ> last (final7 6000)
--    Cerrada
--    (0.02 secs, 2,391,760 bytes)
--
--    λ> last (final3 5000000)
--    Cerrada
--    (0.10 secs, 800,601,824 bytes)
--    λ> last (final4 5000000)
--    Cerrada
--    (0.13 secs, 800,623,184 bytes)
--    λ> last (final5 5000000)
--    Cerrada
--    (1.69 secs, 1,800,604,760 bytes)
--    λ> last (final6 5000000)
--    Cerrada
--    (0.10 secs, 800,628,664 bytes)
--    λ> last (final7 5000000)
--    Cerrada
--    (1.79 secs, 1,481,013,016 bytes)
--
--    λ> last (final3 100000000)
--    Abierta
--    (1.76 secs, 16,000,601,840 bytes)
--    λ> last (final4 100000000)
--    Abierta
--    (1.78 secs, 16,000,626,912 bytes)
--    λ> last (final6 100000000)
--    Abierta
--    (1.76 secs, 16,000,641,600 bytes)
