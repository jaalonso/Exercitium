-- BEE_Reinas.hs
-- El problema de las n reinas (mediante espacios de estados).
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-junio-2023
-- ---------------------------------------------------------------------

module BEE_Reinas where

-- ---------------------------------------------------------------------
-- El problema de las n reinas consiste en colocar n reinas en un
-- tablero cuadrado de dimensiones n por n de forma que no se encuentren
-- más de una en la misma línea: horizontal, vertical o diagonal.
--
-- Las posiciones de las reinas en el tablero se representan por su
-- columna y su fila.
--    type Columna = Int
--    type Fila    = Int
--
-- Una solución del problema de las n reinas es una lista de
-- posiciones.
--    type SolNR = [(Columna,Fila)]
--
-- Usnado el procedimiento de búsqueda en profundidad, definir las
-- funciones
--    solucionesNR      :: Columna -> [SolNR]
--    primeraSolucionNR :: Columna -> SolNR
--    nSolucionesNR     :: Columna -> Int
-- tales que
-- + (solucionesNR n) es la lista de las soluciones del problema de las n
--   reinas, por búsqueda de espacio de estados en profundidad. Por
--   ejemplo,
--      take 3 (solucionesNR 8)
--      [[(1,1),(2,5),(3,8),(4,6),(5,3),(6,7),(7,2),(8,4)],
--       [(1,1),(2,6),(3,8),(4,3),(5,7),(6,4),(7,2),(8,5)],
--       [(1,1),(2,7),(3,4),(4,6),(5,8),(6,2),(7,5),(8,3)]]
-- + (primeraSolucionNR n) es la primera solución del problema de las n
--   reinas, por búsqueda en espacio de estados por profundidad. Por
--   ejemplo,
--      λ> primeraSolucionNR 8
--      [(1,1),(2,5),(3,8),(4,6),(5,3),(6,7),(7,2),(8,4)]
-- + (nSolucionesNR n) es el número de soluciones del problema de las n
--   reinas, por búsqueda en espacio de estados. Por ejemplo,
--      nSolucionesNR 8  ==  92
-- ---------------------------------------------------------------------

import BusquedaEnProfundidad (buscaProfundidad)
import Test.Hspec (Spec, hspec, it, shouldBe)

type Columna = Int
type Fila    = Int
type SolNR = [(Columna,Fila)]

solucionesNR :: Columna -> [SolNR]
solucionesNR n =
  map estado (buscaProfundidad sucesoresNR esFinalNR (1,n,[]))
  where
    estado (_,_,e) = e

primeraSolucionNR :: Columna -> SolNR
primeraSolucionNR =
  head . solucionesNR

nSolucionesNR :: Columna -> Int
nSolucionesNR =
  length . solucionesNR

-- (valida sp p) se verifica si la posición p es válida respecto de la
-- solución parcial sp; es decir, la reina en la posición p no amenaza a
-- ninguna de las reinas de la sp (se supone que están en distintas
-- columnas). Por ejemplo,
--    valida [(1,1)] (2,2)  ==  False
--    valida [(1,1)] (2,3)  ==  True
valida :: SolNR -> (Columna,Fila) -> Bool
valida solp (c,r) = and [test s | s <- solp]
  where test (c',r') = c'+r'/=c+r && c'-r'/=c-r && r'/=r

-- Los nodos del problema de las n reinas son ternas formadas por la
-- columna de la última reina colocada, el número de columnas del
-- tablero y la solución parcial de las reinas colocadas anteriormente.
type NodoNR = (Columna,Columna,SolNR)

-- (sucesoresNR e) es la lista de los sucesores del estado e en el
-- problema de las n reinas. Por ejemplo,
--    λ> sucesoresNR (1,4,[])
--    [(2,4,[(1,1)]),(2,4,[(1,2)]),(2,4,[(1,3)]),(2,4,[(1,4)])]
sucesoresNR :: NodoNR -> [NodoNR]
sucesoresNR (c,n,solp) =
  [(c+1,n,solp ++ [(c,r)]) | r <- [1..n] , valida solp (c,r)]

-- (esFinalNR e) se verifica si e es un estado final del problema de las
-- n reinas.
esFinalNR :: NodoNR -> Bool
esFinalNR (c,n,_) = c > n

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    take 3 (solucionesNR 8) `shouldBe`
    [[(1,1),(2,5),(3,8),(4,6),(5,3),(6,7),(7,2),(8,4)],
     [(1,1),(2,6),(3,8),(4,3),(5,7),(6,4),(7,2),(8,5)],
     [(1,1),(2,7),(3,4),(4,6),(5,8),(6,2),(7,5),(8,3)]]
  it "e2" $
    nSolucionesNR 8 `shouldBe` 92

-- La verificación es
--    λ> verifica
