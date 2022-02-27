primosConsecutivosConMediaCapicua :: [(Integer,Integer,Integer)]
primosConsecutivosConMediaCapicua =
  [(x,y,z) | (x,y) <- zip primosImpares (tail primosImpares),
             let z = (x + y) `div` 2,
             capicua z]

primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

primosImpares :: [Integer]
primosImpares = [x | x <- [3,5..], primo x]

capicua :: Integer -> Bool
capicua x = ys == reverse ys
  where ys = show x
