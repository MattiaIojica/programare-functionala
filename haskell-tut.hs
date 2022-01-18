module Tutorial where
import Data.List
import System.IO
import Text.XHtml.Transitional (face)


main = do
    putStrLn "what's your name"
    name <- getLine 
    putStrLn ("hello " ++ name)

addMe :: Int -> Int -> Int 
--funcName param1 param2 = operations(return value)

addMe x y = x + y


sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int ) -> (Int, Int)
addTuples (x,y) (x2, y2) = (x +  x2, y + y2)

whatAge :: Int -> String 
whatAge 14 = "bravo, ai 14 ani"
whatAge 16 = "permis de motocicleta"
whatAge 18 = "You can drive"
whatAge _ = "nimic important"


--factorial

fact :: Int -> Int 
fact 0 = 1
fact n = n * fact (n - 1)

--sau

prodFact n = product [2..n]

isOdd :: Int -> Bool 
isOdd n
    | n `mod` 2 == 0 = False 
    | otherwise = True


getl :: [Int] -> String 
getl [] = "Your list is empty"
getl (x:[]) = "Your list starts with " ++ show x
getl (x:y: []) ="Your list contains " ++ show x
getl(x : xs) = "the 1 item is" ++ show x ++ " and the second is" ++ show xs


getFirst :: String -> String 
getFirst[] = "empty string"
getFirst all@(x:xs) = "the first letter in " ++ all ++ " is " ++ [x]

ori4 :: Int -> Int 
ori4 x = x * 4

listTimes4 = map ori4[1,2,3,4]

ml :: [Int] -> [Int]
ml[] = []
ml(x:xs) = ori4 x : ml xs


areStringsEq:: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _= False

--primeste o functie

dm :: (Int -> Int) -> Int 
dm func = func 3
nt4 = dm ori4

gadd :: Int -> (Int -> Int)
gadd x y = x + y

adds3 = gadd 3

fourplus3 = adds3 4

