-- El_problema_del_calendario_mediante_busqueda_en_espacio_de_estado.hs
-- El problema del calendario mediante búsqueda en espacio de estado.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema del calendario, para una competición deportiva en la que
-- se enfrentan n participantes, consiste en elaborar un calendario de
-- forma que:
--    + el campeonato dure n-1 días,
--    + cada participante juegue exactamente un partido diario y
--    + cada participante juegue exactamente una vez con cada adversario.
-- Por ejemplo, con 8 participantes una posible solución es
--      | 1 2 3 4 5 6 7
--    --+--------------
--    1 | 2 3 4 5 6 7 8
--    2 | 1 4 3 6 5 8 7
--    3 | 4 1 2 7 8 5 6
--    4 | 3 2 1 8 7 6 5
--    5 | 6 7 8 1 2 3 4
--    6 | 5 8 7 2 1 4 3
--    7 | 8 5 6 3 4 1 2
--    8 | 7 6 5 4 3 2 1
-- donde las filas indican los jugadores y las columnas los días; es
-- decir, el elemento (i,j) indica el adversario del jugador i el día j;
-- por ejemplo, el adversario del jugador 2 el 4ª día es el jugador 6.
--
-- Prara representar el problema se define el tipo Calendario como
-- matrices de enteros
--    type Calendario = Matrix Int
--
-- Usando el [procedimiento de búsqueda en profundidad](https://bit.ly/3NPI4qV),
-- definir la función
--    calendario :: Int -> [Calendario]
-- tal que (calendario n) son las soluciones del problema del calendario,
-- con n participantes, mediante el patrón de búsqueda em
-- profundidad. Por ejemplo,
--    λ> head (calendario 6)
--    ┌           ┐
--    │ 2 3 4 5 6 │
--    │ 1 4 5 6 3 │
--    │ 5 1 6 4 2 │
--    │ 6 2 1 3 5 │
--    │ 3 6 2 1 4 │
--    │ 4 5 3 2 1 │
--    └           ┘
--
--    λ> length (calendario 6)
--    720
--    λ> length (calendario 5)
--    0
-- ---------------------------------------------------------------------

module El_problema_del_calendario_mediante_busqueda_en_espacio_de_estado where

import BusquedaEnProfundidad (buscaProfundidad)
import Data.Matrix (Matrix, (!), nrows, zero, setElem, toLists)
import Data.List ((\\))
import Test.Hspec (Spec, hspec, it, shouldBe)

type Calendario = Matrix Int

-- (inicial n) es el estado inicial para el problema del calendario con
-- n participantes; es decir, una matriz de n fila y n-1 columnas con
-- todos sus elementos iguales a 0. Por ejemplo,
--    λ> inicial 4
--    ┌       ┐
--    │ 0 0 0 │
--    │ 0 0 0 │
--    │ 0 0 0 │
--    │ 0 0 0 │
--    └       ┘
inicial :: Int -> Calendario
inicial n = zero n (n-1)

-- (huecos c) es la lista de las posiciones de c cuyo valor es 0.
huecos :: Calendario -> [(Int, Int)]
huecos c = [(i,j) | i <- [1..n], j <- [1..n-1], c!(i,j) == 0]
  where n = nrows c

-- (sucesores c) es la lista de calendarios obtenidos poniendo en el
-- lugar del primer elemento nulo de c uno de los posibles jugadores de
-- forma que se cumplan las condiciones del problema. Por ejemplo,
--    λ> sucesores (inicial 4)
--    [┌       ┐  ┌       ┐  ┌       ┐
--     │ 2 0 0 │  │ 3 0 0 │  │ 4 0 0 │
--     │ 1 0 0 │  │ 0 0 0 │  │ 0 0 0 │
--     │ 0 0 0 │  │ 1 0 0 │  │ 0 0 0 │
--     │ 0 0 0 │  │ 0 0 0 │  │ 1 0 0 │
--     └       ┘, └       ┘, └       ┘]
--    λ> sucesores (fromLists [[2,3,0],[1,0,0],[0,1,0],[0,0,0]])
--    [┌       ┐
--     │ 2 3 4 │
--     │ 1 0 0 │
--     │ 0 1 0 │
--     │ 0 0 1 │
--     └       ┘]
--    λ> sucesores (fromLists [[2,3,4],[1,0,0],[0,1,0],[0,0,1]])
--    [┌       ┐
--     │ 2 3 4 │
--     │ 1 4 0 │
--     │ 0 1 0 │
--     │ 0 2 1 │
--     └       ┘]
sucesores :: Calendario -> [Calendario]
sucesores c =
  [setElem i (k,j) (setElem k (i,j) c) |
   k <- [1..n] \\ (i : [c!(k,j) | k <- [1..i-1]] ++
                       [c!(i,k) | k <- [1..j-1]]),
   c!(k,j) == 0]
  where
    n = nrows c
    (i,j) = head (huecos c)

-- (esFfinal c) se verifica si c un estado final para el problema
-- del calendario con n participantes; es decir, no queda en c ningún
-- elemento igual a 0. Por ejemplo,
--    λ> esFinal (fromLists [[2,3,4],[1,4,3],[4,1,2],[3,2,1]])
--    True
--    λ> esFinal (fromLists [[2,3,4],[1,4,3],[4,1,2],[3,2,0]])
--    False
esFinal :: Calendario -> Bool
esFinal c = null (huecos c)

calendario :: Int -> [Calendario]
calendario n = buscaProfundidad sucesores esFinal (inicial n)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    toLists (head (calendario 6)) `shouldBe`
    [[2,3,4,5,6],[1,4,5,6,3],[5,1,6,4,2],[6,2,1,3,5],[3,6,2,1,4],[4,5,3,2,1]]
  it "e2" $
    length (calendario 6) `shouldBe` 720
  it "e3" $
    length (calendario 5) `shouldBe` 0

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--
--    Finished in 0.2580 seconds
--    3 examples, 0 failures
