-- Grafo_Algoritmo_de_Kruskal.hs
-- TAD de los grafos: Algoritmo de Kruskal.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El [algoritmo de Kruskal](https://bit.ly/3N8bOOg) calcula un árbol
-- recubridor mínimo en un grafo conexo y ponderado. Es decir, busca un
-- subconjunto de aristas que, formando un árbol, incluyen todos los
-- vértices y donde el valor de la suma de todas las aristas del árbol
-- es el mínimo.
--
-- El algoritmo de Kruskal funciona de la siguiente manera:
-- + se crea un bosque B (un conjunto de árboles), donde cada vértice
--   del grafo es un árbol separado
-- + se crea un conjunto C que contenga a todas las aristas del grafo
-- + mientras C es no vacío,
--   + eliminar una arista de peso mínimo de C
--   + si esa arista conecta dos árboles diferentes se añade al bosque,
--     combinando los dos árboles en un solo árbol
--   + en caso contrario, se desecha la arista
-- Al acabar el algoritmo, el bosque tiene un solo componente, el cual
-- forma un árbol de expansión mínimo del grafo.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    kruskal :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
-- tal que (kruskal g) es el árbol de expansión mínimo del grafo g calculado
-- mediante el algoritmo de Kruskal. Por ejemplo, si g1, g2, g3 y g4 son
-- los grafos definidos por
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
--    kruskal g1  ==  [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
--    kruskal g2  ==  [(32,2,5),(13,1,2),(12,2,4),(11,1,3)]
--    kruskal g3  ==  [(9,5,7),(7,2,3),(6,1,6),(5,4,5),(5,1,2),(3,5,6)]
--    kruskal g4  ==  [(9,5,7),(6,1,6),(5,4,5),(5,1,2),(3,5,6),(1,3,5)]
-- ---------------------------------------------------------------------

module Grafo_Algoritmo_de_Kruskal where
import TAD.Grafo (Grafo, Orientacion (ND), aristas, creaGrafo, nodos)
import Data.Ix (Ix)
import qualified Data.Map as M (Map, (!), fromList, keys, update)
import Data.List (sort)
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

kruskal :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
kruskal g = aux (sort [(p,x,y) | ((x,y),p) <- aristas g])
                (M.fromList [(x,x) | x <- nodos g])
                []
                (length (nodos g) - 1)
  where aux _  _ ae 0 = ae
        aux [] _ _  _ = error "Imposible"
        aux ((p,x,y):as) d ae n
          | actualizado = aux as d' ((p,x,y):ae) (n-1)
          | otherwise   = aux as d ae n
          where (actualizado,d') = buscaActualiza (x,y) d

-- (raiz d n) es la raíz de n en el diccionario. Por ejemplo,
--    raiz (M.fromList [(1,1),(3,1),(4,3),(5,4),(2,6),(6,6)]) 5  == 1
--    raiz (M.fromList [(1,1),(3,1),(4,3),(5,4),(2,6),(6,6)]) 2  == 6
raiz :: (Eq n, Ord n) => M.Map n n -> n -> n
raiz d x | v == x    = v
         | otherwise = raiz d v
  where v = d M.! x

-- (buscaActualiza a d) es el par formado por False y el diccionario d,
-- si los dos vértices de la arista a tienen la misma raíz en d y el par
-- formado por True y la tabla obtenida añadiéndole a d la arista
-- formada por el vértice de a de mayor raíz y la raíz del vértice de a
-- de menor raíz. Y actualizando las raices de todos los elementos
-- afectados por la raíz añadida. Por ejemplo,
--   λ> d = M.fromList [(1,1),(2,1),(3,3),(4,4),(5,5),(6,5),(7,7)]
--   λ> buscaActualiza (5,4) d
--   (True,fromList [(1,1),(2,1),(3,3),(4,4),(5,4),(6,4),(7,7)])
--   λ> d' = snd it
--   λ> buscaActualiza (6,1) d'
--   (True,fromList [(1,1),(2,1),(3,3),(4,1),(5,1),(6,1),(7,7)])
buscaActualiza :: (Eq n, Ord n) => (n,n) -> M.Map n n -> (Bool,M.Map n n)
buscaActualiza (x,y) d
  | x' == y'  = (False, d)
  | y' <  x'  = (True, modificaR x (d M.! x) y' d)
  | otherwise = (True, modificaR y (d M.! y) x' d)
  where x' = raiz d x
        y' = raiz d y

-- (modificaR x y y' d) actualiza d como sigue:
-- + el valor de todas las claves z con valor y es y'
-- + el valor de todas las claves z con (z > x) con valor x es y'
modificaR :: (Eq n, Ord n) => n -> n -> n -> M.Map n n -> M.Map n n
modificaR x y y' d = aux2 ds (aux1 cs d)
  where cs = M.keys d
        ds = filter (>x) cs
        aux1 [] tb = tb
        aux1 (a:as) tb | tb M.! a == y = aux1 as (M.update (\_ -> Just y') a tb)
                       | otherwise     = aux1 as tb
        aux2 [] tb = tb
        aux2 (b:bs) tb | tb M.! b == x = aux2 bs (M.update (\_ -> Just y') b tb)
                       | otherwise     = aux2 bs tb

-- Traza del diccionario correspondiente al grafo g3
-- =================================================

-- Lista de aristas, ordenadas según su peso:
-- [(3,5,6),(5,1,2),(5,4,5),(6,1,6),(7,2,3),(7,3,5),(8,3,4),(9,1,3),(9,5,7),(11,6,7),(15,1,5)]
--
-- Inicial
--   fromList [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
--
-- Después de añadir la arista (5,6) de peso 3
--   fromList [(1,1),(2,2),(3,3),(4,4),(5,5),(6,5),(7,7)]
--
-- Después de añadir la arista (1,2) de peso 5
--   fromList [(1,1),(2,1),(3,3),(4,4),(5,5),(6,5),(7,7)]
--
-- Después de añadir la arista (4,5) de peso 5
--   fromList [(1,1),(2,1),(3,3),(4,4),(5,4),(6,4),(7,7)]
--
-- Después de añadir la arista (1,6) de peso 6
--   fromList [(1,1),(2,1),(3,3),(4,1),(5,1),(6,1),(7,7)]
--
-- Después de añadir la arista (2,3) de peso 7
--   fromList [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,7)]
--
-- Las posibles aristas a añadir son:
-- + la (3,5) con peso 7, que no es posible pues la raíz de 3
--   coincide con la raíz de 5, por lo que formaría un ciclo
-- + la (3,4) con peso 8, que no es posible pues la raíz de 3
--   coincide con la raíz de 4, por lo que formaría un ciclo
-- + la (1,3) con peso 9, que no es posible pues la raíz de 3
--   coincide con la raíz de 1, por lo que formaría un ciclo
-- + la (5,7) con peso 9, que no forma ciclo
--
-- Después de añadir la arista (5,7) con peso 9
--    fromList [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1)]
--
-- No es posible añadir más aristas, pues formarían ciclos.

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    kruskal g1 `shouldBe` [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
  it "e2" $
    kruskal g2 `shouldBe` [(32,2,5),(13,1,2),(12,2,4),(11,1,3)]
  it "e3" $
    kruskal g3 `shouldBe` [(9,5,7),(7,2,3),(6,1,6),(5,4,5),(5,1,2),(3,5,6)]
  it "e4" $
    kruskal g4 `shouldBe` [(9,5,7),(6,1,6),(5,4,5),(5,1,2),(3,5,6),(1,3,5)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0044 seconds
--    4 examples, 0 failures
