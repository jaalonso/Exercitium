-- El_problema_del_domino.hs
-- El problema del dominó.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las fichas del dominó se pueden representar por pares de números
-- enteros. El problema del dominó consiste en colocar todas las fichas
-- de una lista dada de forma que el segundo número de cada ficha
-- coincida con el primero de la siguiente.
--
-- Definir, mediante búsqueda en espacio de estados, la función
--    domino :: [(Int,Int)] -> [[(Int,Int)]]
-- tal que (domino fs) es la lista de las soluciones del problema del
-- dominó correspondiente a las fichas fs. Por ejemplo,
--    λ> domino [(1,2),(2,3),(1,4)]
--    [[(4,1),(1,2),(2,3)],[(3,2),(2,1),(1,4)]]
--    λ> domino [(1,2),(1,1),(1,4)]
--    [[(4,1),(1,1),(1,2)],[(2,1),(1,1),(1,4)]]
--    λ> domino [(1,2),(3,4),(2,3)]
--    [[(1,2),(2,3),(3,4)],[(4,3),(3,2),(2,1)]]
--    λ> domino [(1,2),(2,3),(5,4)]
--    []
-- ---------------------------------------------------------------------

module El_problema_del_domino where

import BusquedaEnProfundidad (buscaProfundidad)
import Data.List (delete)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- Las fichas son pares de números enteros.
type Ficha  = (Int,Int)

-- Un problema está definido por la lista de fichas que hay que colocar
type Problema = [Ficha]

-- Los estados son los pares formados por la listas sin colocar y las
-- colocadas.
type Estado = ([Ficha],[Ficha])

-- (inicial p) es el estado inicial del problema p.
inicial :: Problema -> Estado
inicial p = (p,[])

-- (esFinal e) se verifica si e es un estado final.
esFinal :: Estado -> Bool
esFinal = null . fst

sucesores :: Estado -> [Estado]
sucesores (fs,[]) =
  [(delete (a,b) fs, [(a,b)]) | (a,b) <- fs, a /= b] ++
  [(delete (a,b) fs, [(b,a)]) | (a,b) <- fs]
sucesores (fs,e@((x,_):_)) =
  [(delete (u,v) fs,(u,v):e) | (u,v) <- fs, u /= v, v == x] ++
  [(delete (u,v) fs,(v,u):e) | (u,v) <- fs, u /= v, u == x] ++
  [(delete (u,v) fs,(u,v):e) | (u,v) <- fs, u == v, u == x]

soluciones :: Problema -> [Estado]
soluciones p = buscaProfundidad sucesores esFinal (inicial p)

domino :: Problema -> [[Ficha]]
domino p = map snd (soluciones p)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    domino [(1,2),(2,3),(1,4)] `shouldBe`
    [[(4,1),(1,2),(2,3)],[(3,2),(2,1),(1,4)]]
  it "e2" $
    domino [(1,2),(1,1),(1,4)] `shouldBe`
    [[(4,1),(1,1),(1,2)],[(2,1),(1,1),(1,4)]]
  it "e3" $
    domino [(1,2),(3,4),(2,3)] `shouldBe`
    [[(1,2),(2,3),(3,4)],[(4,3),(3,2),(2,1)]]
  it "e4" $
    domino [(1,2),(2,3),(5,4)] `shouldBe`
    []

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0013 seconds
--    4 examples, 0 failures
