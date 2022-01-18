module Curs5 where
import Data.List
import Data.Char
import Data.Foldable


rev :: [a] -> [a]
rev xs = revl xs []
    where 
        revl [] l = l
        revl (x:xs) rxs = revl xs (x : rxs)
--rev xs = foldr(\ x u xs' -> u (x:xs)) id xs []

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

summ :: [Int] -> Int
summ xs = foldr (\ x u n -> u (n + x)) id xs 0


primes = sieve[2..]
sieve(p : ps) = p : sieve[x | x <- ps, mod x p /= 0]


--1
firstEl :: [(a, b)] -> [a]
firstEl = map fst


--2
sumList :: [[Int]] -> [Int]
sumList = map sum

--3
prel2 :: [Int] -> [Int]
prel2 = map(\x -> if even x then div x 2 else x * 2)

--4
fooChar :: Char -> [String] -> [String]
fooChar c = filter (elem c)

--5
ex5 :: [Int] -> [Int]
ex5 l = map (^2) (filter odd l)

--6
ex6 :: [Int] -> [Int]
ex6 l = map((^2) .snd) (filter(odd . fst) (zip [1..length l] l))

--ex7
numaiVocale :: [String] -> [String]
numaiVocale = map(filter(`elem` "aeoiu"))

-- 8
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) = if p x then x : myFilter p xs else myFilter p xs

-- 9
sumSquares :: [Int] -> Int
sumSquares list = sum (map (^2) (filter odd list))

-- 10
allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- 11
--a)
rmChar :: Char -> String -> String
rmChar ch string = [c | c <- string, c /= ch]

--b)
rmCharsRec :: String -> String -> String 
rmCharsRec _ [] = []
rmCharsRec unwanted (x:xs)
  | x `notElem` unwanted = x: rmCharsRec unwanted xs
  | otherwise            = rmCharsRec unwanted xs

--c)
rmCharsFold :: String -> String -> String
rmCharsFold unwanted = foldr (\c -> if c `notElem` unwanted then (c:)
  else id) []