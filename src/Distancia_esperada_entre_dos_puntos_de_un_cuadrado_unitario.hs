-- Distancia_esperada_entre_dos_puntos_de_un_cuadrado_unitario.hs
-- Distancia esperada entre dos puntos de un cuadrado unitario.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir, por simulación, la función
--    distanciaEsperada :: Int -> IO Double
-- tal que (distanciaEsperada n) es la distancia esperada entre n puntos
-- del cuadrado unitario de vértices opuestos (0,0) y (1,1), elegidos
-- aleatoriamente. Por ejemplo,
--    distanciaEsperada 10     ==  0.43903617921423593
--    distanciaEsperada 10     ==  0.6342350621260004
--    distanciaEsperada 100    ==  0.5180418995364429
--    distanciaEsperada 100    ==  0.5288261085653962
--    distanciaEsperada 1000   ==  0.5143804432569616
--    distanciaEsperada 10000  ==  0.5208360147922616
--
-- El valor exacto de la distancia esperada es 
--    ve = (sqrt(2) + 2 + 5*log(1+sqrt(2)))/15 = 0.5214054331647207
-- 
-- Definir la función 
--    graficaDistanciaEsperada :: [Int] -> IO ()
-- tal que (graficaDistanciaEsperadan n) dibuja las gráficas de los
-- pares (n, distanciaEsperada n) para n en la lista creciente ns junto
-- con la recta y = ve, donde ve es el valor exacto. Por ejemplo,
-- (graficaDistanciaEsperada [10,30..4000]) dibuja  
--    Distancia_esperada_entre_dos_puntos.png
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Distancia_esperada_entre_dos_puntos_de_un_cuadrado_unitario where

import Data.List     (genericLength)
import System.Random (newStdGen, randomRIO, randomRs)
import Control.Monad (replicateM)
import Graphics.Gnuplot.Simple

-- 1ª solución
-- ===========

-- Un punto es un par de números reales.
type Punto = (Double, Double)

-- (puntosDelCuadrado n) es una lista de n puntos del cuadrado
-- unitario de vértices opuestos (0,0) y (1,1). Por ejemplo, 
--    λ> puntosDelCuadrado 3
--    [(0.6067427807212623,0.24785843546479303),
--     (0.9579158098726746,8.047408846191773e-2),
--     (0.856758357789639,0.9814972717003113)]
--    λ> puntosDelCuadrado 3
--    [(1.9785720974027532e-2,0.6343219201012211),
--     (0.21903717179861604,0.20947986189590784),
--     (0.4739903340716357,1.2262474491489095e-2)]
puntosDelCuadrado :: Int -> IO [Punto]
puntosDelCuadrado n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
      (as, ys) = splitAt n xs
      (bs, _)  = splitAt n ys
  return (zip as bs)
  
-- (distancia p1 p2) es la distancia entre los puntos p1 y p2. Por
-- ejemplo,
--    distancia (0,0) (3,4)  ==  5.0
distancia :: Punto -> Punto -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

-- (distancias ps) es la lista de las distancias entre los elementos 1º
-- y 2º, 3º y 4º, ... de ps. Por ejemplo,
--    distancias [(0,0),(3,4),(1,1),(7,9)]  ==  [5.0,10.0]
distancias :: [Punto] -> [Double]
distancias []         = []
distancias (p1:p2:ps) = distancia p1 p2 : distancias ps

-- (media xs) es la media aritmética de los elementos de xs. Por ejemplo,
--    media [1,7,1]  ==  3.0
media :: [Double] -> Double
media xs =
  sum xs / genericLength xs

-- (distanciaEsperada n) es la distancia esperada entre n puntos
-- aleatorios en el cuadrado unitario. Por ejemplo,
--    λ> distanciaEsperada 100
--    0.4712858421448363
--    λ> distanciaEsperada 100
--    0.4947745206856711
distanciaEsperada :: Int -> IO Double
distanciaEsperada n = do
  ps <- puntosDelCuadrado (2*n)
  return (media (distancias ps))

-- 2ª solución
-- ===========

distanciaEsperada2 :: Int -> IO Double
distanciaEsperada2 n = do
  ps <- puntosDelCuadrado2 (2*n)
  return (media (distancias ps))

-- (puntosDelCuadrado2 n) es una lista de n puntos del cuadrado
-- unitario de vértices opuestos (0,0) y (1,1). Por ejemplo, 
--    λ> puntosDelCuadrado2 3
--    [(0.9836699352638695,0.5143414844876929),
--     (0.8715237339877027,0.9905157772823782),
--     (0.29502946161912935,0.16889248111565192)]
--    λ> puntosDelCuadrado2 3
--    [(0.20405570457106392,0.47574116941605116),
--     (0.7128182811364226,3.201419787777959e-2),
--     (0.5576891231675457,0.9994474730919443)]
puntosDelCuadrado2 :: Int -> IO [Punto]
puntosDelCuadrado2 n =
  replicateM n puntoDelCuadrado2

-- (puntoDelCuadrado2 n) es un punto del cuadrado unitario de vértices
-- opuestos (0,0) y (1,1). Por ejemplo,  
--    λ> puntoDelCuadrado2
--    (0.7512991739803923,0.966436016138578)
--    λ> puntoDelCuadrado2
--    (0.7306826194847795,0.8984574498515252)
puntoDelCuadrado2 :: IO Punto
puntoDelCuadrado2 = do
  x <- randomRIO (0, 1.0)
  y <- randomRIO (0, 1.0)
  return (x, y)

-- 3ª solución
-- ===========

distanciaEsperada3 :: Int -> IO Double
distanciaEsperada3 n = do
  ds <- distanciasAleatorias n
  return (media ds)

-- (distanciasAleatorias n) es la lista de las distancias aleatorias
-- entre n pares de puntos del cuadrado unitario. Por ejemplo, 
--    λ> distanciasAleatorias 3
--    [0.8325589110989705,0.6803336613847881,0.1690051224111662]
--    λ> distanciasAleatorias 3
--    [0.3470124940889039,0.459002678562019,0.7665623634969365]
distanciasAleatorias :: Int -> IO [Double]
distanciasAleatorias n = 
  replicateM n distanciaAleatoria

-- distanciaAleatoria es la distancia de un par de punto del cuadrado
-- unitario elegidos aleatoriamente. Por ejemplo,
--    λ> distanciaAleatoria
--    0.8982361685460913
--    λ> distanciaAleatoria
--    0.9777207485571939
--    λ> distanciaAleatoria
--    0.6042223512347842
distanciaAleatoria :: IO Double
distanciaAleatoria = do 
  p1 <- puntoDelCuadrado2
  distancia p1 <$> puntoDelCuadrado2

-- 4ª solución
-- ===========

distanciaEsperada4 :: Int -> IO Double
distanciaEsperada4 n =
  media <$> distanciasAleatorias n

-- Gráfica
-- =======

graficaDistanciaEsperada :: [Int] -> IO ()
graficaDistanciaEsperada ns = do
  ys <- mapM distanciaEsperada ns
  let e = (sqrt 2 + 2 + 5 * log (1 + sqrt 2)) / 15
  plotLists [ Key Nothing
            -- , PNG "Distancia_esperada_entre_dos_puntos.png"
            ]
            [ zip ns ys
            , zip ns (repeat e)]
