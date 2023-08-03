-- BEE_El_problema_del_granjero.hs
-- El problema del granjero mediante búsqueda en espacio de estado.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un granjero está parado en un lado del río y con él tiene un lobo,
-- una cabra y una repollo. En el río hay un barco pequeño. El granjero
-- desea cruzar el río con sus tres posesiones. No hay puentes y en el
-- barco hay solamente sitio para el granjero y un artículo. Si deja
-- la cabra con la repollo sola en un lado del río la cabra comerá la
-- repollo. Si deja el lobo y la cabra en un lado, el lobo se comerá a
-- la cabra. ¿Cómo puede cruzar el granjero el río con los tres
-- artículos, sin que ninguno se coma al otro?
--
-- Para representar el problema se definen los siguientes tipos de dato:
-- + Orilla con dos constructores I y D que representan las orillas
--   izquierda y derecha, respectivamente.
-- + Estado que es una tupla que representa en qué orilla se encuentra
--   cada uno de los elementos (granjero, lobo, cabra, repollo). Por
--   ejemplo, (I,D,D,I) representa que el granjero está en la izquierda,
--   que el lobo está en la derecha, que la cabra está en la derecha y
--   el repollo está en la izquierda.
--
-- Usando el [procedimiento de búsqueda en profundidad](https://bit.ly/3NPI4qV),
-- definir la función
--    granjero :: [[Estado]]
-- tal que granjero son las soluciones del problema del granjero
-- mediante el patrón de búsqueda en espacio de estados. Por ejemplo,
--    λ> head granjero
--    [(I,I,I,I),(D,I,D,I),(I,I,D,I),(D,D,D,I),
--     (I,D,I,I),(D,D,I,D),(I,D,I,D),(D,D,D,D)]
--    λ> length granjero
--    2
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module BEE_El_problema_del_granjero where

import BusquedaEnProfundidad (buscaProfundidad)
import Test.Hspec (Spec, hspec, it, shouldBe)

data Orilla = I | D
  deriving (Eq, Show)

type Estado = (Orilla,Orilla,Orilla,Orilla)

-- (seguro e) se verifica si el estado e es seguro; es decir, que no
-- puede estar en una orilla el lobo con la cabra sin el granjero ni la
-- cabra con el repollo sin el granjero. Por ejemplo,
--    seguro (I,D,D,I)  ==  False
--    seguro (D,D,D,I)  ==  True
--    seguro (D,D,I,I)  ==  False
--    seguro (I,D,I,I)  ==  True
seguro :: Estado -> Bool
seguro (g,l,c,r) = not (g /= c && (c == l || c == r))

-- (opuesta x) es la opuesta de la orilla x. Por ejemplo
--    opuesta I = D
opuesta :: Orilla -> Orilla
opuesta I = D
opuesta D = I

-- (sucesoresE e) es la lista de los sucesores seguros del estado e. Por
-- ejemplo,
--    sucesoresE (I,I,I,I)  ==  [(D,I,D,I)]
--    sucesoresE (D,I,D,I)  ==  [(I,I,D,I),(I,I,I,I)]
sucesoresE :: Estado -> [Estado]
sucesoresE e = [mov e | mov <- [m1,m2,m3,m4], seguro (mov e)]
  where m1 (g,l,c,r) = (opuesta g, l, c, r)
        m2 (g,l,c,r) = (opuesta g, opuesta l, c, r)
        m3 (g,l,c,r) = (opuesta g, l, opuesta c, r)
        m4 (g,l,c,r) = (opuesta g, l, c, opuesta r)

-- Nodo es el tipo de los nodos del espacio de búsqueda, donde un nodo
-- es una lista de estados
--    [e_n, ..., e_2, e_1]
-- tal que e_1 es el estado inicial y para cada i (2 <= i <= n), e_i es un
-- sucesor de e_(i-1).
newtype Nodo = Nodo [Estado]
  deriving (Eq, Show)

-- inicial es el nodo inicial en el que todos están en la orilla
-- izquierda.
inicial :: Nodo
inicial = Nodo [(I,I,I,I)]

-- (esFinal n) se verifica si n es un nodo final; es decir, su primer
-- elemento es el estado final. Por ejemplo,
--    esFinal (Nodo [(D,D,D,D),(I,I,I,I)])  ==  True
--    esFinal (Nodo [(I,I,D,I),(I,I,I,I)])  ==  False
esFinal :: Nodo -> Bool
esFinal (Nodo (n:_)) = n == (D,D,D,D)

-- (sucesores n) es la lista de los sucesores del nodo n. Por ejemplo,
--    λ> sucesores (Nodo [(I,I,D,I),(D,I,D,I),(I,I,I,I)])
--    [Nodo [(D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)],
--     Nodo [(D,I,D,D),(I,I,D,I),(D,I,D,I),(I,I,I,I)]]
sucesores :: Nodo -> [Nodo]
sucesores (Nodo n@(e:es)) =
  [Nodo (e':n) | e' <- sucesoresE e, e' `notElem` es]

granjero :: [[Estado]]
granjero =
  [reverse es | (Nodo es) <- buscaProfundidad sucesores esFinal inicial]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    head granjero `shouldBe`
    [(I,I,I,I),(D,I,D,I),(I,I,D,I),(D,D,D,I),
     (I,D,I,I),(D,D,I,D),(I,D,I,D),(D,D,D,D)]
  it "e2" $
    length granjero `shouldBe` 2

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0008 seconds
--    2 examples, 0 failures
