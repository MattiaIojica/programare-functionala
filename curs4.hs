module Curs4 where
import Data.List
import Data.Char
import Data.Foldable

squares :: [Int] -> [Int]
squares l = [x * x | x <- l]

ords :: [Char] -> [Int]
ords l = [ord x | x <- l]

mapp :: (a -> b) -> [a] -> [b]
mapp f xs = [f x | x <- xs]


sq :: [Int] -> [Int]
sq xs = map sqr xs
    where sqr x = x * x

pozitive :: [Int] -> [Int]
pozitive xs = filter poz xs
    where poz x = x > 0

concatt :: [[a]] -> [a]
concatt [] = []
concatt (x : xs) = x ++ concatt xs

sss :: [Int] -> Int
sss l = foldr (+) 0 l


f :: [Int] -> Int
f xs = foldr (+) 0 (map sqr (filter pos xs))
    where 
        sqr x = x * x
        pos x = x > 0

strs = ["cezar", "petru", "", "andrei"];

maxLength = foldr max 0 .
        map length .
        filter testC
    where 
        testC ('c':_) = True
        testC _ = False

maxLengthF = maxLength strs

factori :: Int -> [Int]
factori x = [n | n <- [1..x `div` 2] , x `mod` n == 0] ++ [x]

prim :: Int -> Bool
prim 1 = False
prim 2 = True
prim l 
    |(length [x | x <- [2], l `mod` x == 0]) > 0 = False
    |(length [x | x <- [3], l `mod` x == 0]) > 0 = False
    |(length [x | x <- [5, 11..(l `div` 2)], l `mod` x == 0]) > 0 = False
    |otherwise = True




