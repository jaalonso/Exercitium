-- Base_de_dato_de_actividades.hs
-- Base de dato de actividades.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las bases de datos sobre actividades de personas pueden representarse
-- mediante listas de elementos de la forma (a,b,c,d), donde a es el
-- nombre de la persona, b su actividad, c su fecha de nacimiento y d la
-- de su fallecimiento. Un ejemplo es la siguiente que usaremos a lo
-- largo de este ejercicio,
--    personas :: [(String,String,Int,Int)]
--    personas = [("Cervantes","Literatura",1547,1616),
--                ("Velazquez","Pintura",1599,1660),
--                ("Picasso","Pintura",1881,1973),
--                ("Beethoven","Musica",1770,1823),
--                ("Poincare","Ciencia",1854,1912),
--                ("Quevedo","Literatura",1580,1654),
--                ("Goya","Pintura",1746,1828),
--                ("Einstein","Ciencia",1879,1955),
--                ("Mozart","Musica",1756,1791),
--                ("Botticelli","Pintura",1445,1510),
--                ("Borromini","Arquitectura",1599,1667),
--                ("Bach","Musica",1685,1750)]
--
-- Definir las funciones
--    nombres   :: [(String,String,Int,Int)] -> [String]
--    musicos   :: [(String,String,Int,Int)] -> [String]
--    seleccion :: [(String,String,Int,Int)] -> String -> [String]
--    musicos'  :: [(String,String,Int,Int)] -> [String]
--    vivas     :: [(String,String,Int,Int)] -> Int -> [String]
-- tales que
-- + (nombres bd) es la lista de los nombres de las personas de la- base
--   de datos bd. Por ejemplo,
--      λ> nombres personas
--      ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
--       "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini",
--       "Bach"]
-- + (musicos bd) es la lista de los nombres de los músicos de la base
--   de datos bd. Por ejemplo,
--      musicos personas  ==  ["Beethoven","Mozart","Bach"]
-- + (seleccion bd m) es la lista de los nombres de las personas de la
--   base de datos bd cuya actividad es m. Por ejemplo,
--      λ> seleccion personas "Pintura"
--      ["Velazquez","Picasso","Goya","Botticelli"]
--      λ> seleccion personas "Musica"
--      ["Beethoven","Mozart","Bach"]
-- + (musicos' bd) es la lista de los nombres de los músicos de la base
--   de datos bd. Por ejemplo,
--      musicos' personas  ==  ["Beethoven","Mozart","Bach"]
-- + (vivas bd a) es la lista de los nombres de las personas de la base
--   de datos bd  que estaban vivas en el año a. Por ejemplo,
--      λ> vivas personas 1600
--      ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

module Base_de_dato_de_actividades where

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [x | (x,_,_,_) <- bd]

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [x | (x,"Musica",_,_) <- bd]

seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [ x | (x,m',_,_) <- bd, m == m' ]

musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas bd a = [x | (x,_,a1,a2) <- bd, a1 <= a, a <= a2]
