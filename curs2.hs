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

--Laborator

--1
poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x =
  let x1 = x*x
  in a*x1 + b*x + c

poly :: Double -> Double -> Double -> Double
poly a b x = a*x + b

--2
eeny :: Integer -> String
eeny x
 | even x = "eeny"
 | otherwise = "meeny"

--3.1, cu if
fizzbuzz1 :: Integer -> String
fizzbuzz1 x =
  if x `mod` 15 == 0 then
     "FizzBuzz"
  else
    if x `mod` 3 == 0 then
      "Fizz"
    else
      if x `mod` 5 == 0 then
        "Buzz"
      else
        ""

--3.2, cu garzi
fizzbuzz2 :: Integer -> String
fizzbuzz2 x
  | x `mod` 15 == 0 = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = ""

--4.1, cu cazuri
tribonacci1 :: Integer -> Integer
tribonacci1 x
  | x < 1 = 0
  | x < 3 = 1
  | x == 3 = 2
  | otherwise = tribonacci1 (x-1) + tribonacci1 (x-2) + tribonacci1 (x-3)

--4.2, ecuational
tribonacci2 :: Integer -> Integer
tribonacci2 0 = 0
tribonacci2 1 = 1
tribonacci2 2 = 1
tribonacci2 3 = 2
tribonacci2 x = tribonacci2 (x-1) + tribonacci2 (x-2) + tribonacci2 (x-3)

--5
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

--6 a
verifL :: [Int] -> Bool
verifL a
  | length a `mod` 2 == 0 = True
  | otherwise = False

--6 b
takefinal :: [Int] -> Int -> [Int]
takefinal [] _ = []
takefinal (x:xs) n
  | length (x:xs) <= n = (x:xs)
  | otherwise = takefinal xs n

--6 b.2
takefinalS :: String -> Int -> String
takefinalS "" _ = ""
takefinalS (x:xs) n
  | length (x:xs) <= n = (x:xs)
  | otherwise = takefinalS xs n

--6 c
remove :: [Int] -> Int -> [Int]
remove xs n = take (n-1) xs ++ drop n xs

--7 a
myreplicate :: Int -> Double -> [Double]
myreplicate 0 _ = []
myreplicate n v
  | n > 0 = v : myreplicate (n-1) v

--7 b
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs)
  | x `mod` 2 == 1 = x + sumImp xs
  | otherwise = sumImp xs

--7 c
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs)
  | head x == 'A' = length x + totalLen xs
  | otherwise = totalLen xs