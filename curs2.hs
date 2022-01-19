module Curs2 where
import Data.List

--folosind if
fact :: Int -> Int
fact n = if n == 0 then 1
    else n * fact(n - 1)

--folosind ecuatii
fact1 :: Int -> Int
fact1 0 = 1
fact1 n = n * fact1(n -1)

--folosind cazuri
fact2 :: Int -> Int
fact2 n
    |n == 0 = 1
    |otherwise = n * fact2(n - 1)

--Liste
-- [1,2,3] == 1:(2:(3:[])) == 1:2:3:[]

-- "abcd" == ['a'..'d'] == 'a':('b':('c':('d':[]))) == 'a':'b':'c':'d':[]

--lista de nr pare
pare = [x | x <- [1..10], even x]


--zip
lll =[(i, x) | (i, x) <- [1..] `zip` ['A'..'Z'], even i]

zzz = [(x, y) | x <- [1..4], y <- ['a'..'i']]

natural = [0..]