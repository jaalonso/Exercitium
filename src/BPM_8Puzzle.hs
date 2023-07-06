-- BPM_8Puzzle.hs
-- El problema del 8 puzzle.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-julio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Para el 8-puzzle se usa un cajón cuadrado en el que hay situados 8
-- bloques cuadrados. El cuadrado restante está sin rellenar. Cada
-- bloque tiene un número. Un bloque adyacente al hueco puede deslizarse
-- hacia él. El juego consiste en transformar la posición inicial en la
-- posición final mediante el deslizamiento de los bloques. En
-- particular, consideramos el estado inicial y final siguientes:
--
--    +---+---+---+                   +---+---+---+
--    |   | 1 | 3 |                   | 1 | 2 | 3 |
--    +---+---+---+                   +---+---+---+
--    | 8 | 2 | 4 |                   | 8 |   | 4 |
--    +---+---+---+                   +---+---+---+
--    | 7 | 5 | 5 |                   | 7 | 6 | 5 |
--    +---+---+---+                   +---+---+---+
--    Estado inicial                  Estado final
--
-- Para solucionar el problema se definen los siguientes tipos:
-- + Tablero es matriz de número enteros (que representan las piezas en
--   cada posición y el 0 representa el hueco):
--      type Tablero  = Matrix Int
-- + Estado es una listas de tableros [t_n,...,t_1] tal que t_i es un
--   sucesor de t_(i-1).
--      newtype Estado = Est [Tablero]
--        deriving Show
--
-- Usando el procedimiento de [búsqueda por primero el mejor](???),
-- definir la función
--    solucion_8puzzle :: Tablero -> [Tablero]
-- tal que (solucion_8puzzle t) es la solución del problema del problema
-- del 8 puzzle a partir del tablero0 t. Por ejemplo,
--    λ> solucion_8puzzle (fromLists [[0,1,3],[8,2,4],[7,6,5]])
--    [┌       ┐  ┌       ┐  ┌       ┐
--     │ 0 1 3 │  │ 1 0 3 │  │ 1 2 3 │
--     │ 8 2 4 │  │ 8 2 4 │  │ 8 0 4 │
--     │ 7 6 5 │  │ 7 6 5 │  │ 7 6 5 │
--     └       ┘, └       ┘, └       ┘]
--    λ> length (solucion_8puzzle (fromLists [[2,6,3],[5,0,4],[1,7,8]]))
--    21
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module BPM_8Puzzle where

import BusquedaPrimeroElMejor (buscaPM)
import Data.Matrix (Matrix, (!), fromLists, setElem, toLists)
import Test.Hspec (Spec, hspec, it, shouldBe)

type Tablero  = Matrix Int

newtype Estado = Est [Tablero]
  deriving (Eq, Show)

solucion_8puzzle :: Tablero -> [Tablero]
solucion_8puzzle t = reverse ts
  where (Est ts) = head (buscaPM sucesores
                                 esFinal
                                 (inicial t))

-- Estado inicial
-- ==============

-- (inicial t) es el estado inicial del problema del 8 puzzle a partir del
-- tablero t.
inicial :: Tablero -> Estado
inicial t = Est [t]

-- Estado final
-- ============

-- (esFinal e) se verifica si e es un estado final.
esFinal :: Estado -> Bool
esFinal (Est (n:_)) = n == tableroFinal

-- tableroFinal es el estado tablero final del 8 puzzle.
tableroFinal :: Tablero
tableroFinal = fromLists [[1,2,3],
                          [8,0,4],
                          [7,6,5]]

-- Sucesores
-- =========

-- (sucesores e) es la lista de sucesores del estado e. Por ejemplo,
--    λ> sucesores (Est [fromLists [[2,1,3],[8,0,4],[7,6,5]]])
--    [Est [┌       ┐  ┌       ┐
--          │ 2 0 3 │  │ 2 1 3 │
--          │ 8 1 4 │  │ 8 0 4 │
--          │ 7 6 5 │  │ 7 6 5 │
--          └       ┘, └       ┘],
--     Est [┌       ┐  ┌       ┐
--          │ 2 1 3 │  │ 2 1 3 │
--          │ 8 6 4 │  │ 8 0 4 │
--          │ 7 0 5 │  │ 7 6 5 │
--          └       ┘, └       ┘],
--     Est [┌       ┐  ┌       ┐
--          │ 2 1 3 │  │ 2 1 3 │
--          │ 0 8 4 │  │ 8 0 4 │
--          │ 7 6 5 │  │ 7 6 5 │
--          └       ┘, └       ┘],
--     Est [┌       ┐  ┌       ┐
--          │ 2 1 3 │  │ 2 1 3 │
--          │ 8 4 0 │  │ 8 0 4 │
--          │ 7 6 5 │  │ 7 6 5 │
--          └       ┘, └       ┘]]
sucesores :: Estado -> [Estado]
sucesores (Est e@(t:_)) =
  [Est (t':e) | t' <- tablerosSucesores t,
                t' `notElem` e]

-- (tablerosSucesores t) es la lista de los tableros sucesores del
-- tablero t. Por ejemplo,
--    λ> tablerosSucesores (fromLists [[2,1,3],[8,0,4],[7,6,5]])
--    [┌       ┐  ┌       ┐  ┌       ┐  ┌       ┐
--     │ 2 0 3 │  │ 2 1 3 │  │ 2 1 3 │  │ 2 1 3 │
--     │ 8 1 4 │  │ 8 6 4 │  │ 0 8 4 │  │ 8 4 0 │
--     │ 7 6 5 │  │ 7 0 5 │  │ 7 6 5 │  │ 7 6 5 │
--     └       ┘, └       ┘, └       ┘, └       ┘]
tablerosSucesores :: Tablero -> [Tablero]
tablerosSucesores t =
  [intercambia t p q | q <- posicionesVecinas p]
  where p = posicionHueco t

-- Una posición es un par de enteros.
type Posicion = (Int,Int)

-- (posicionesVecinas p) son las posiciones de la matriz cuadrada de
-- dimensión 3 que se encuentran encima, abajo, a la izquierda y a la
-- derecha de los posición p. Por ejemplo,
--    λ> posicionesVecinas (2,2)
--    [(1,2),(3,2),(2,1),(2,3)]
--    λ> posicionesVecinas (1,2)
--    [(2,2),(1,1),(1,3)]
--    λ> posicionesVecinas (1,1)
--    [(2,1),(1,2)]
posicionesVecinas :: Posicion -> [Posicion]
posicionesVecinas (i,j) =
  [(i-1,j) | i > 1] ++
  [(i+1,j) | i < 3] ++
  [(i,j-1) | j > 1] ++
  [(i,j+1) | j < 3]

-- (posicionHueco t) es la posición del hueco en el tablero t. Por
-- ejemplo,
--    λ> posicionHueco (fromLists [[2,1,3],[8,0,4],[7,6,5]])
--    (2,2)
posicionHueco :: Tablero -> Posicion
posicionHueco t =
  posicionElemento t 0

-- (posicionElemento t a) es la posición de elemento a en el tablero
-- t. Por ejemplo,
--    λ> posicionElemento (fromLists [[2,1,3],[8,0,4],[7,6,5]]) 4
--    (2,3)
posicionElemento :: Tablero -> Int -> Posicion
posicionElemento t a =
  head [(i,j) | i <- [1..3],
                j <- [1..3],
                t ! (i,j) == a]

-- (intercambia t p1 p2) es el tablero obtenido intercambiando en t los
-- elementos que se encuentran en las posiciones p1 y p2. Por ejemplo,
--    λ> intercambia (fromLists [[2,1,3],[8,0,4],[7,6,5]]) (1,2) (2,2)
--    ┌       ┐
--    │ 2 0 3 │
--    │ 8 1 4 │
--    │ 7 6 5 │
--    └       ┘
intercambia :: Tablero -> Posicion -> Posicion -> Tablero
intercambia t p1 p2 =
  setElem a2 p1 (setElem a1 p2 t)
  where a1 = t ! p1
        a2 = t ! p2

-- Heurística
-- ==========

-- (heuristica t) es la suma de la distancia Manhatan desde la posición de
-- cada objeto del tablero a su posición en el tablero final. Por
-- ejemplo,
--    λ> heuristica (fromLists [[0,1,3],[8,2,4],[7,6,5]])
--    4
heuristica :: Tablero  -> Int
heuristica t =
  sum [distancia (posicionElemento t i)
                 (posicionElemento tableroFinal i)
      | i <- [0..8]]

-- (distancia p1 p2) es la distancia Manhatan entre las posiciones p1 y
-- p2. Por ejemplo,
--    distancia (2,7) (4,1)  ==  8
distancia :: Posicion -> Posicion -> Int
distancia (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- Comparación de estados
-- ======================

-- Un estado es menor o igual que otro si tiene la heurística de su
-- primer tablero es menor o que la del segundo o so iguales y el
-- primero es más corto.
instance Ord Estado where
  Est (t1:ts1) <= Est (t2:ts2) = (heuristica t1 < heuristica t2) ||
                                 ((heuristica t1 == heuristica t2) &&
                                  (length ts1 <= length ts2))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    map toLists (solucion_8puzzle (fromLists [[0,1,3],[8,2,4],[7,6,5]]))
   `shouldBe` [[[0,1,3],
                [8,2,4],
                [7,6,5]],
               [[1,0,3],
                [8,2,4],
                [7,6,5]],
               [[1,2,3],
                [8,0,4],
                [7,6,5]]]
  it "e2" $
    length (solucion_8puzzle (fromLists [[2,6,3],[5,0,4],[1,7,8]]))
    `shouldBe` 21

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.1361 seconds
--    2 examples, 0 failures
