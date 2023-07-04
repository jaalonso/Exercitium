-- BPM_8Puzzle.hs
-- El problema del 8 puzzle.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-julio-2023
-- ---------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module BPM_8Puzzle where

import BusquedaPrimeroElMejor
import Data.Array
import Data.List (sort)
import Data.Tuple (swap)
import Data.List.Split (chunksOf)

-- Representación del problema:
-- ============================

-- Una posición es un par de enteros.
type Posicion = (Int,Int)

-- Un tablero es un vector de posiciones, en el que el índice indica el
-- elemento que ocupa la posición.
type Tablero  = Array Int Posicion


-- inicial8P es el estado inicial del 8 puzzle. En el ejemplo es
--      +---+---+---+
--      | 2 | 6 | 3 |
--      +---+---+---+
--      | 5 |   | 4 |
--      +---+---+---+
--      | 1 | 7 | 8 |
--      +---+---+---+
inicial8P :: Tablero
inicial8P = array (0,8) [(2,(1,3)),(6,(2,3)),(3,(3,3)),
                         (5,(1,2)),(0,(2,2)),(4,(3,2)),
                         (1,(1,1)),(7,(2,1)),(8,(3,1))]

-- final8P es el estado final del 8 puzzle. En el ejemplo es
--      +---+---+---+
--      | 1 | 2 | 3 |
--      +---+---+---+
--      | 8 |   | 4 |
--      +---+---+---+
--      | 7 | 6 | 5 |
--      +---+---+---+
final8P :: Tablero
final8P = array (0,8) [(1,(1,3)),(2,(2,3)),(3,(3,3)),
                       (8,(1,2)),(0,(2,2)),(4,(3,2)),
                       (7,(1,1)),(6,(2,1)),(5,(3,1))]

-- (escribeTablero t) escribe el tablero t. Por ejemplo,
--    λ> escribeTablero inicial8P
--    152
--    706
--    843
--
--    λ> escribeTablero final8P
--    781
--    602
--    543
escribeTablero :: Tablero -> IO()
escribeTablero t =
  putStrLn (concatMap (\xs -> concatMap show xs ++ "\n") xss)
  where xss = chunksOf 3 (map snd (sort (map swap (assocs t))))


-- (distancia p1 p2) es la distancia Manhatan entre las posiciones p1 y
-- p2. Por ejemplo,
--    distancia (2,7) (4,1)  ==  8
distancia :: Posicion -> Posicion -> Int
distancia (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- (adyacente p1 p2) se verifica si las posiciones p1 y p2 son
-- adyacentes. Por ejemplo,
--    adyacente (3,2) (3,1)  ==  True
--    adyacente (3,2) (1,2)  ==  False
adyacente :: Posicion -> Posicion -> Bool
adyacente p1 p2 = distancia p1 p2 == 1

-- (todosMovimientos t) es la lista de los tableros obtenidos
-- aplicándole al tablero t todos los posibles movimientos; es decir,
-- intercambiando la posición del hueco con sus adyacentes. Por ejemplo,
--    λ> escribeTablero inicial8P
--    152
--    706
--    843
--
--    λ> mapM_ escribeTablero (todosMovimientos inicial8P)
--    152
--    746
--    803
--
--    102
--    756
--    843
--
--    152
--    760
--    843
--
--    152
--    076
--    843
todosMovimientos :: Tablero -> [Tablero]
todosMovimientos t = [t//[(0,t!i),(i,t!0)] | i<-[1..8], adyacente (t!0) (t!i)]

-- Los nodos del espacio de estados son listas de tableros [t_n,...,t_1]
-- tal que t_i es un sucesor de t_(i-1).
newtype Tableros = Est [Tablero] deriving Show

-- (escribeTableros e) escribe los tableros del estado e.
escribeTableros :: Tableros -> IO()
escribeTableros (Est ts) = do
  mapM_ escribeTablero (reverse ts)
  putStr "---\n\n"

-- (sucesores8P e) es la lista de sucesores del estado e. Por ejemplo,
--    λ> mapM_ escribeTableros (sucesores8P (Est [inicial8P]))
--    152
--    706
--    843
--
--    152
--    746
--    803
--
--    ---
--
--    152
--    706
--    843
--
--    102
--    756
--    843
--
--    ---
--
--    152
--    706
--    843
--
--    152
--    760
--    843
--
--    ---
--
--    152
--    706
--    843
--
--    152
--    076
--    843
--
--    ---
sucesores8P :: Tableros -> [Tableros]
sucesores8P (Est n@(t:ts)) =
  filter (noEn ts) [ Est (t':n) | t' <- todosMovimientos t]
  where noEn ts' (Est(t':_)) = elems t' `notElem` map elems ts'

esFinal8P :: Tableros -> Bool
esFinal8P (Est (n:_)) = elems n == elems final8P

-- Heurísticas
-- ===========

-- (heur1 t) es la suma de la distancia Manhatan desde la posición de
-- cada objeto del tablero a su posición en el estado final. Por
-- ejemplo,
--    heur1 inicial8P  ==  12
heur1 :: Tablero  -> Int
heur1 b = sum [distancia (b!i) (final8P!i) | i <- [0..8]]

-- Dos estados se consideran iguales si tienen la misma heurística.
instance Eq Tableros where
  Est(t1:_) == Est(t2:_) = heur1 t1 == heur1 t2
  _         == _         = error "Imposible"

-- Un estado es menor o igual que otro si tiene una heurística menor o
-- igual.
instance Ord Tableros where
  Est (t1:_) <= Est (t2:_) = heur1 t1 <= heur1 t2

-- (soluciones_8P) es la lista de las soluciones del 8 puzzle por búsqueda
-- primero el mejor. Por ejemplo,
--    λ> escribeTableros (head soluciones_8P)
--    152
--    706
--    843
--
--    102
--    756
--    843
--
--    012
--    756
--    843
--
--    712
--    056
--    843
--
--    712
--    506
--    843
--
--    712
--    560
--    843
--
--    710
--    562
--    843
--
--    701
--    562
--    843
--
--    071
--    562
--    843
--
--    571
--    062
--    843
--
--    571
--    862
--    043
--
--    571
--    862
--    403
--
--    571
--    802
--    463
--
--    571
--    082
--    463
--
--    071
--    582
--    463
--
--    701
--    582
--    463
--
--    781
--    502
--    463
--
--    781
--    562
--    403
--
--    781
--    562
--    043
--
--    781
--    062
--    543
--
--    781
--    602
--    543
--
--    ---
soluciones_8P :: [Tableros]
soluciones_8P = buscaPM sucesores8P
                        esFinal8P
                        (Est [inicial8P])
