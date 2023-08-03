-- Escalada_Prim.hs
-- El algoritmo de Prim del árbol de expansión mínimo por escalada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El [algoritmo de Prim](https://bit.ly/466fwRe) calcula un árbol
-- recubridor mínimo en un grafo conexo y ponderado. Es decir, busca un
-- subconjunto de aristas que, formando un árbol, incluyen todos los
-- vértices y donde el valor de la suma de todas las aristas del árbol
-- es el mínimo.
--
-- El algoritmo de Prim funciona de la siguiente manera:
-- + Inicializar un árbol con un único vértice, elegido arbitrariamente,
--   del grafo.
-- + Aumentar el árbol por un lado. Llamamos lado a la unión entre dos
--   vértices: de las posibles uniones que pueden conectar el árbol a los
--   vértices que no están aún en el árbol, encontrar el lado de menor
--   distancia y unirlo al árbol.
-- + Repetir el paso 2 (hasta que todos los vértices pertenezcan al
--   árbol)
--
-- Usando la [búsqueda en escalada](https://bit.ly/3Kk4A99) el [tipo
-- abstracto de datos de los grafos](https://bit.ly/45cQ3Fo), definir la
-- función
--    prim :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
-- tal que (prim g) es el árbol de expansión mínimo del grafo g
-- calculado mediante el algoritmo de Prim con bñusqueda en
-- escalada. Por ejemplo, si g1, g2, g3 y g4 son los grafos definidos
-- por
--    g1, g2, g3, g4 :: Grafo Int Int
--    g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
--                             (2,4,55),(2,5,32),
--                             (3,4,61),(3,5,44),
--                             (4,5,93)]
--    g2 = creaGrafo ND (1,5) [(1,2,13),(1,3,11),(1,5,78),
--                             (2,4,12),(2,5,32),
--                             (3,4,14),(3,5,44),
--                             (4,5,93)]
--    g3 = creaGrafo ND (1,7) [(1,2,5),(1,3,9),(1,5,15),(1,6,6),
--                             (2,3,7),
--                             (3,4,8),(3,5,7),
--                             (4,5,5),
--                             (5,6,3),(5,7,9),
--                             (6,7,11)]
--    g4 = creaGrafo ND (1,7) [(1,2,5),(1,3,9),(1,5,15),(1,6,6),
--                             (2,3,7),
--                             (3,4,8),(3,5,1),
--                             (4,5,5),
--                             (5,6,3),(5,7,9),
--                             (6,7,11)]
-- entonces
--    prim g1 == [(2,4,55),(1,3,34),(2,5,32),(1,2,12)]
--    prim g2 == [(2,5,32),(2,4,12),(1,2,13),(1,3,11)]
--    prim g3 == [(5,7,9),(2,3,7),(5,4,5),(6,5,3),(1,6,6),(1,2,5)]
--    prim g4 == [(5,7,9),(5,4,5),(5,3,1),(6,5,3),(1,6,6),(1,2,5)]
-- ---------------------------------------------------------------------

module Escalada_Prim where

import BusquedaEnEscalada (buscaEscalada)
import TAD.Grafo (Grafo, Orientacion (ND), aristaEn, creaGrafo, nodos, peso)
import Data.Ix (Ix)
import Data.List (delete)
import Test.Hspec (Spec, hspec, it, shouldBe)

g1, g2, g3, g4 :: Grafo Int Int
g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                         (2,4,55),(2,5,32),
                         (3,4,61),(3,5,44),
                         (4,5,93)]
g2 = creaGrafo ND (1,5) [(1,2,13),(1,3,11),(1,5,78),
                         (2,4,12),(2,5,32),
                         (3,4,14),(3,5,44),
                         (4,5,93)]
g3 = creaGrafo ND (1,7) [(1,2,5),(1,3,9),(1,5,15),(1,6,6),
                         (2,3,7),
                         (3,4,8),(3,5,7),
                         (4,5,5),
                         (5,6,3),(5,7,9),
                         (6,7,11)]
g4 = creaGrafo ND (1,7) [(1,2,5),(1,3,9),(1,5,15),(1,6,6),
                         (2,3,7),
                         (3,4,8),(3,5,1),
                         (4,5,5),
                         (5,6,3),(5,7,9),
                         (6,7,11)]

-- Una arista esta formada por dos vértices junto con su peso.
type Arista a b = (a,a,b)

-- Un estado (Estado (p,t,r,aem)) está formado por el peso p de la
-- última arista añadida el árbol de expansión mínimo (aem), la lista t
-- de nodos del grafo que están en el aem, la lista r de nodos del
-- grafo que no están en el aem y el aem.
type Estado a b = (b,[a],[a],[Arista a b])

-- (inicial g) es el estado inicial correspondiente al grafo g.
inicial :: (Ix a, Num b, Ord b) => Grafo a b -> Estado a b
inicial g = (0,[n],ns,[])
  where (n:ns) = nodos g

-- (esFinal e) se verifica si e es un estado final; es decir, si no
-- queda ningún elemento en la lista de nodos sin colocar en el árbol de
-- expansión mínimo.
esFinal :: Estado a b -> Bool
esFinal (_,_,[],_) = True
esFinal _          = False

-- (sucesores g e) es la lista de los sucesores del estado e en el
-- grafo g. Por ejemplo,
--    λ> sucesores g1 (0,[1],[2..5],[])
--    [(12,[2,1],[3,4,5],[(1,2,12)]),
--     (34,[3,1],[2,4,5],[(1,3,34)]),
--     (78,[5,1],[2,3,4],[(1,5,78)])]
sucesores
  :: (Ix a, Num b, Eq b) => Grafo a b -> Estado a b -> [Estado a b]
sucesores g (_,t,r,aem) =
  [(peso x y g, y:t, delete y r, (x,y,peso x y g):aem)
   | x <- t , y <- r, aristaEn g (x,y)]

prim :: (Ix a, Num b, Ord b) => Grafo a b -> [Arista a b]
prim g = sol
  where [(_,_,_,sol)] = buscaEscalada (sucesores g)
                                      esFinal
                                      (inicial g)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    prim g1 `shouldBe` [(2,4,55),(1,3,34),(2,5,32),(1,2,12)]
  it "e2" $
    prim g2 `shouldBe` [(2,5,32),(2,4,12),(1,2,13),(1,3,11)]
  it "e3" $
    prim g3 `shouldBe` [(5,7,9),(2,3,7),(5,4,5),(6,5,3),(1,6,6),(1,2,5)]
  it "e4" $
    prim g4 `shouldBe` [(5,7,9),(5,4,5),(5,3,1),(6,5,3),(1,6,6),(1,2,5)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0043 seconds
--    4 examples, 0 failures
