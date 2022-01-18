module Curs3 where
import Data.List
import Data.Char




f :: (Int, String) -> String
f (n, s) = take n s

foo :: (Int,(Char,String)) -> String
foo (a, (b, c)) = ""

foo3 :: Int -> Char -> String
foo3 a b = ""

-- map :: (a -> b) -> [a] -> [b]
-- map f l = [f x | x <- l]

-- filter :: (a -> Bool) -> [a] -> [b]
-- filter p l = [x | x <- l, p x]

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs) = (length $ filter isVowel x) -- + nrVocale(xs)

adaugaNr :: Int -> [Int] -> [Int]
adaugaNr nr [] = []
adaugaNr nr (x:xs)
    | even x = x:nr: adaugaNr nr xs
    | otherwise = x: adaugaNr nr xs

semiPareComp :: [Int] -> [Int]
semiPareComp l = [x `div` 2 | x <- l, even x]


divizori :: Int -> [Int]
divizori l = [x | x <- [1..l], l `mod` x == 0]

listaDiv :: [Int] -> [[Int]]
listaDiv l = [divizori x | x <- l]

inInterval :: Int -> Int -> [Int] -> [Int]
-- inInterval a b l = [x | x <- l, x >= a && x <= b]

inInterval a b [] = []
inInterval a b (x:xs)
    |x < a = inInterval a b xs
    |x > b = inInterval a b xs
    |otherwise = x:inInterval a b xs

pozitive :: [Int] -> Int
pozitive [] = 0
pozitive (x:xs)
    |x >= 0 = 1 + pozitive xs
    |otherwise = pozitive xs

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare l = [l !! n | n <- [0..length l - 1], odd n]


multDigits :: String -> Int
multDigits "" = 1
multDigits (x:xs)
    |elem x "1234567890" = (digitToInt x) * multDigits xs
    |otherwise = 1 * multDigits xs

-- multDigits2 :: String -> Int
-- multDigits2 s = [(digitToInt x) | x <- s, isDigit x]