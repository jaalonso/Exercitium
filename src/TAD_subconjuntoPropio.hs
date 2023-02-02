-- TAD_subconjuntoPropio.hs
-- TAD de los conjuntos: Reconocimiento de_subconjunto propio
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir la función
--    subconjuntoPropio :: Ord a => Conj a -> Conj a -> Bool
-- tal (subconjuntoPropio c1 c2) se verifica si c1 es un subconjunto
-- propio de c2. Por ejemplo,
--    λ> ej1 = inserta 5 (inserta 2 vacio)
--    λ> ej2 = inserta 3 (inserta 2 (inserta 5 vacio))
--    λ> ej3 = inserta 3 (inserta 4 (inserta 5 vacio))
--    λ> ej4 = inserta 2 (inserta 5 vacio)
--    λ> subconjuntoPropio ej1 ej2
--    True
--    λ> subconjuntoPropio ej1 ej3
--    False
--    λ> subconjuntoPropio ej1 ej4
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_subconjuntoPropio where

import TAD.Conjunto (Conj, vacio, inserta)
import TAD_subconjunto (subconjunto)

subconjuntoPropio :: Ord a => Conj a -> Conj a -> Bool
subconjuntoPropio c1 c2 =
  subconjunto c1 c2 && c1 /= c2

-- La función subconjunto está definida en el ejercicio
-- "Reconocimiento de subconjuntos" que se encuentra en
-- https://bit.ly/3wPBtU5
