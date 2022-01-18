module Curs6 where
import Data.List
import Data.Char
import Data.Foldable
import GHC.Maybe


data Bool = False | True

-- not :: Bool -> Bool
-- not False = True
-- not True = False

data Seasons = Spring | Summer | Autumn | Winter
succesor :: Seasons -> Seasons
succesor Spring = Summer
succesor Summer = Autumn
succesor Autumn = Winter
succesor Winter = Spring

showSeason :: Seasons -> String
showSeason Spring = "Primavara"
showSeason Summer = "Vara"
showSeason Autumn = "Toamna"
showSeason Winter = "Iarna"


data Point a b = Pt a b

pr1 :: Point a b -> a
pr1 (Pt x _) = x
pr2 :: Point a b -> b
pr2 (Pt _ y) = y

pointFlip :: Point a b -> Point b a
pointFlip (Pt x y) = Pt y x

data List a = Nil
    | Cons a (List a)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

data StrInt = VS String | VI Int
-- [VI 1, VS "abc", VI 34, VI 0, VS "xyz"] :: [StrInt]

-- data Seasons = Winter | Spring | Summer | Fall

data Shape = Circle Float | Rectangle Float Float

data Pair a b = Pair a b 

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp

-- data List a = Nil | Cons a (List a)

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)

-- data [a] = [] | a : [a]

-- data (a,b) = (a,b)
-- data (a,b,c) = (a,b,c)

data Nat = Zero | Succ Nat

(^^^) :: Float -> Nat -> Float
x ^^^ Zero = 1.0
x ^^^ (Succ n) = x * (x ^^^ n)

(^^^^) :: Float -> Int -> Float
x ^^^^ 0 = 1.0
x ^^^^ n = x * (x ^^^^ (n - 1))

data Maybee a = Nothingg | Justt a

divide :: Int -> Int -> Maybee Int
divide n 0 = Nothingg
divide n m = Justt (div n m)

type FirstName = String
type LastName = String
type Age = Int
type Height = Float
type Phone = String

data Person = Person FirstName LastName Age Height Phone

firstName :: Person -> String
firstName (Person firstname _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _) = age

height :: Person -> Float
height (Person _ _ _ height _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number) = number

-- data Person = Person {
--     firstName :: String,
--     lastName :: String,
--     age :: Int,
--     height :: Float,
--     phoneNumber :: String
-- }

-- nextYear person = person { age = age person + 1}


--LAB

--1
rotate :: Int -> [Char] -> [Char]
rotate n (h:t) 
    | n > 0 && n < (length (h:t)) = rotate (n-1) (t ++ (h : []))
    | n < 0 || n > (length (h:t)) = error "n < 0 sau n > lung listei"
    | otherwise = (h:t)

--2
-- prop_rotate :: Int -> String -> Bool
-- prop_rotate k str = rotate (l - m) (rotate m str) == str
--     where l = length str 
--           m = if l == 0 then 0
--              else k `mod` l

--3
makeKey :: Int -> [(Char, Char)]
makeKey n = [(x, y) | (x, y) <- zip ['A' .. 'Z'] (rotate n ['A' .. 'Z'])]

--4
lookUp :: Char -> [(Char, Char)] -> [Char]
lookUp ch lista = ls
    where ls = [snd (x, y) | (x, y) <- lista, ord ch == ord (fst (x, y))]

--5
encipher :: Int -> Char -> Char
encipher n ch 
    | n > 0 && isAlpha ch = chr(((ord ch + n - ord 'A') `mod` 26) + ord 'A')
    | otherwise = ch

--6
normalize :: String -> String
normalize string = [toUpper(x) | x <- string, isAlphaNum x] 

--7
encipherStr :: Int -> String -> String 
encipherStr n string = [encipher n (toUpper(x)) | x <- string, isAlphaNum x]


--8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey lista = [(y, x) | (x, y) <- lista]

--9
decipher :: Int -> Char -> Char
decipher n ch
    | n > 0 && isAlpha ch = chr(((ord ch + ord 'Z' - n) `mod` 26)+ ord 'A' +1)
    | otherwise = ch

decipherStr :: Int -> String -> String
decipherStr n string = [decipher n (toUpper(x)) | x <- string, isAlphaNum x]