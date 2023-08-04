-- BEE_El_problema_de_las_fichas.hs
-- El problema de las fichas mediante búsqueda en espacio de estado.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Para el problema de las fichas de orden (m,n) se considera un tablero
-- con m+n+1 cuadrados consecutivos.
--
-- Inicialmente, en cada uno de los m primeros cuadrados hay una ficha
-- blanca, a continuación un hueco y  en cada uno de los n últimos
-- cuadrados hay una ficha verde. El objetivo consiste en tener las
-- fichas verdes al principio y las blancas al final.
--
-- Por ejemplo, en el problema de las fichas de orden (3,3) el tablero
-- inicial es
--       +---+---+---+---+---+---+---+
--       | B | B | B |   | V | V | V |
--       +---+---+---+---+---+---+---+
-- y el final es
--       +---+---+---+---+---+---+---+
--       | V | V | V |   | B | B | B |
--       +---+---+---+---+---+---+---+
--
-- Los movimientos permitidos consisten en desplazar una ficha al hueco
-- saltando, como máximo, sobre otras dos.
--
-- Para representar el problema se definen los siguientes tipos de
-- datos:
-- + Ficha con tres constructores B, V y H que representan las fichas
--   blanca, verde y hueco, respectivamente.
--      data Ficha = B | V | H
--        deriving (Eq, Show)
-- + Tablero que es una lista de fichas que representa las fichas
--   colocadas en el tablero.
--      type Tablero = [Ficha]
-- + Estado representa los estados del espacio de búsqueda, donde un
--   estado es una lista de tableros [t_n, ..., t_2, t_1] tal que t_1 es
--   el tablero inicial y para cada i (2 <= i <= n), t_i es un sucesor
--   de t_(i-1).
--      newtype Estado = E [Tablero]
--        deriving (Eq, Show)
-- + Busqueda es un procedimiento de búsqueda
--      type Busqueda = (Estado -> [Estado]) ->
--                      (Estado -> Bool) ->
--                      Estado ->
--                      [Estado]
--
-- Además, se considera la heurística que para cada tablero vale la suma
-- de piezas blancas situadas a la izquierda de cada una de las piezas
-- verdes. Por  ejemplo, para el estado
--       +---+---+---+---+---+---+---+
--       | B | V | B |   | V | V | B |
--       +---+---+---+---+---+---+---+
-- su valor es 1+2+2 = 5. La heurística de un estado es la del primero
-- de sus tableros.
--
-- Usando los métodos de búsqueda estudiado en los ejercicios
-- anteriores, definir la función
--    fichas :: Busqueda -> Int -> Int -> [[Tablero]]
-- tal que (fichas b m n) es la lista de las soluciones del problema de
-- las fichas de orden (m,n) obtenidas mediante el procedimiento de
-- búsqueda b. Por ejemplo,
--    λ> head (fichas buscaProfundidad 2 2)
--    [[B,B,H,V,V],[B,H,B,V,V],[H,B,B,V,V],[V,B,B,H,V],[V,B,H,B,V],[V,H,B,B,V],
--     [H,V,B,B,V],[B,V,H,B,V],[B,H,V,B,V],[H,B,V,B,V],[B,B,V,H,V],[B,B,V,V,H],
--     [B,H,V,V,B],[H,B,V,V,B],[V,B,H,V,B],[V,H,B,V,B],[H,V,B,V,B],[B,V,H,V,B],
--     [B,V,V,H,B],[H,V,V,B,B],[V,H,V,B,B],[V,V,H,B,B]]
--    λ> head (fichas buscaAnchura 2 2)
--    [[B,B,H,V,V],[B,B,V,V,H],[B,H,V,V,B],[B,V,V,H,B],[H,V,V,B,B],[V,V,H,B,B]]
--    λ> head (fichas buscaPM 2 2)
--    [[B,B,H,V,V],[B,H,B,V,V],[B,V,B,H,V],[H,V,B,B,V],[V,H,B,B,V],[V,V,B,B,H],
--     [V,V,B,H,B],[V,V,H,B,B]]
--    λ> head (fichas buscaEscalada 2 2)
--    [[B,B,H,V,V],[B,H,B,V,V],[B,V,B,H,V],[H,V,B,B,V],[V,H,B,B,V],[V,V,B,B,H],
--     [V,V,B,H,B],[V,V,H,B,B]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module BEE_El_problema_de_las_fichas where

import BusquedaEnProfundidad (buscaProfundidad)
import BusquedaEnAnchura (buscaAnchura)
import BusquedaPrimeroElMejor (buscaPM)
import BusquedaEnEscalada (buscaEscalada)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- Representación del problema
-- ===========================

data Ficha = B | V | H
  deriving (Eq, Show)

type Tablero = [Ficha]

-- (tableroInicial m n) representa el tablero inicial del problema de las fichas
-- de orden (m,n). Por ejemplo,
--    tableroInicial 2 3  ==  [B,B,H,V,V,V]
--    tableroInicial 3 2  ==  [B,B,B,H,V,V]
tableroInicial ::  Int -> Int -> Tablero
tableroInicial m n = replicate m B ++ [H] ++ replicate n V

-- (tableroFinal m n) representa el tablero final del problema de las fichas de
-- orden (m,n). Por ejemplo,
--    tableroFinal 2 3  ==  [V,V,V,H,B,B]
--    tableroFinal 3 2  ==  [V,V,H,B,B,B]
tableroFinal ::  Int -> Int -> Tablero
tableroFinal m n = replicate n V ++ [H] ++ replicate m B

-- (tablerosSucesores e) es la lista de los sucesores del tablero e. Por
-- ejemplo,
--    λ> tablerosSucesores [V,B,H,V,V,B]
--    [[V,H,B,V,V,B],[H,B,V,V,V,B],[V,B,V,H,V,B],[V,B,V,V,H,B],
--     [V,B,B,V,V,H]]
--    λ> tablerosSucesores [B,B,B,H,V,V,V]
--    [[B,B,H,B,V,V,V],[B,H,B,B,V,V,V],[H,B,B,B,V,V,V],
--     [B,B,B,V,H,V,V],[B,B,B,V,V,H,V],[B,B,B,V,V,V,H]]
tablerosSucesores :: Tablero -> [Tablero]
tablerosSucesores e =
  [intercambia i j e | i <- [j-1,j-2,j-3,j+1,j+2,j+3]
                     , 0 <= i, i < n]
  where j = posicionHueco e
        n = length e

-- (posicionHueco t) es la posición del hueco en el tablero t. Por
-- ejemplo,
--    posicionHueco tableroInicial  ==  3
posicionHueco :: Tablero -> Int
posicionHueco t = length (takeWhile (/=H) t)

-- (intercambia xs i j) es la lista obtenida intercambiando los
-- elementos de xs en las posiciones i y j. Por ejemplo,
--    intercambia 2 6 [0..9]  ==  [0,1,6,3,4,5,2,7,8,9]
--    intercambia 6 2 [0..9]  ==  [0,1,6,3,4,5,2,7,8,9]
intercambia :: Int -> Int -> [a] -> [a]
intercambia i j xs = concat [xs1,[x2],xs2,[x1],xs3]
  where (xs1,x1,xs2,x2,xs3) = divide (min i j) (max i j) xs

-- (divide xs i j) es la tupla (xs1,x1,xs2,x2,xs3) tal que xs1 son los
-- elementos de xs cuya posición es menor que i, x1 es el elemento de xs
-- en la posición i, xs2 son los elementos de xs cuya posición es mayor
-- que i y menor que j, x2 es el elemento de xs en la posición j y xs3
-- son los elementos de xs cuya posición es mayor que j (suponiendo que
-- i < j). Por ejemplo,
--    divide 2 6 [0..9]  ==  ([0,1],2,[3,4,5],6,[7,8,9])
divide :: Int -> Int -> [a] -> ([a],a,[a],a,[a])
divide i j xs = (xs1,x1,xs2,x2,xs3)
  where (xs1,x1:ys)  = splitAt i xs
        (xs2,x2:xs3) = splitAt (j - i - 1) ys

newtype Estado = E [Tablero]
  deriving (Eq, Show)

-- (inicial m n) representa el estado inicial del problema de las fichas
-- de orden (m,n). Por ejemplo,
--    inicial 2 3  ==  E [[B,B,H,V,V,V]]
--    inicial 3 2  ==  E [[B,B,B,H,V,V]]
inicial :: Int -> Int -> Estado
inicial m n = E [tableroInicial m n]

-- (esFinal m n) se verifica si N es un estado final del problema de las
-- fichas de orden (m,n). Por ejemplo,
--    λ> esFinal 2 1 (E [[V,H,B,B],[V,B,B,H],[H,B,B,V],[B,B,H,V]])
--    True
--    λ> esFinal 2 1 (E [[V,B,B,H],[H,B,B,V],[B,B,H,V]])
--    False
esFinal :: Int -> Int -> Estado -> Bool
esFinal m n (E (e:_)) = e == tableroFinal m n

-- (sucesores n) es la lista de los sucesores del estado n. Por ejemplo,
--    λ> sucesores (E [[H,B,B,V],[B,B,H,V]])
--    [E [[B,H,B,V],[H,B,B,V],[B,B,H,V]],
--     E [[V,B,B,H],[H,B,B,V],[B,B,H,V]]]
--    λ> sucesores (E [[B,H,B,V],[H,B,B,V],[B,B,H,V]])
--    [E [[B,V,B,H],[B,H,B,V],[H,B,B,V],[B,B,H,V]]]
sucesores :: Estado -> [Estado]
sucesores (E n@(e:es)) =
  [E (e':n) | e' <- tablerosSucesores e,
              e' `notElem` es]

-- Heurística
-- ==========

-- (heuristicaT t) es la heurística del tablero t. Por ejemplo,
--    heuristicaT [B,V,B,H,V,V,B] == 5
heuristicaT :: Tablero -> Int
heuristicaT []     = 0
heuristicaT (V:xs) = heuristicaT xs
heuristicaT (H:xs) = heuristicaT xs
heuristicaT (B:xs) = heuristicaT xs + length (filter (==V) xs)

-- (heuristica e) es la heurística del primer tablero del estado e. Por
-- ejemplo,
--    heuristica (E [[H,B,B,V],[B,B,H,V]])            ==  2
--    heuristica (E [[V,B,B,H],[H,B,B,V],[B,B,H,V]])  ==  0
heuristica :: Estado -> Int
heuristica (E (t:_)) = heuristicaT t

-- Estado es un subtipo de Ord de forma que un estado es menor o igual
-- que otro si su heurística lo es.
instance Ord Estado where
  e1 <= e2 = heuristica e1 <= heuristica e2

-- Solución por búsqueda
-- =====================

type Busqueda = (Estado -> [Estado]) ->
                (Estado -> Bool) ->
                Estado ->
                [Estado]

fichas :: Busqueda -> Int -> Int -> [[Tablero]]
fichas b m n =
  [reverse es | E es <- b sucesores (esFinal m n) (inicial m n)]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    head (fichas buscaProfundidad 2 2) `shouldBe`
    [[B,B,H,V,V],[B,H,B,V,V],[H,B,B,V,V],[V,B,B,H,V],[V,B,H,B,V],[V,H,B,B,V],
     [H,V,B,B,V],[B,V,H,B,V],[B,H,V,B,V],[H,B,V,B,V],[B,B,V,H,V],[B,B,V,V,H],
     [B,H,V,V,B],[H,B,V,V,B],[V,B,H,V,B],[V,H,B,V,B],[H,V,B,V,B],[B,V,H,V,B],
     [B,V,V,H,B],[H,V,V,B,B],[V,H,V,B,B],[V,V,H,B,B]]
  it "e2" $
    head (fichas buscaAnchura 2 2) `shouldBe`
    [[B,B,H,V,V],[B,B,V,V,H],[B,H,V,V,B],[B,V,V,H,B],[H,V,V,B,B],[V,V,H,B,B]]
  it "e3" $
    head (fichas buscaPM 2 2) `shouldBe`
    [[B,B,H,V,V],[B,H,B,V,V],[B,V,B,H,V],[H,V,B,B,V],[V,H,B,B,V],[V,V,B,B,H],
     [V,V,B,H,B],[V,V,H,B,B]]
  it "e4" $
    head (fichas buscaEscalada 2 2) `shouldBe`
    [[B,B,H,V,V],[B,H,B,V,V],[B,V,B,H,V],[H,V,B,B,V],[V,H,B,B,V],[V,V,B,B,H],
     [V,V,B,H,B],[V,V,H,B,B]]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0055 seconds
--    4 examples, 0 failures
