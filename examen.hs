module Examen where
import Data.List
import Data.Char
import Data.Foldable

f :: String -> String -> String
f "" "" = ""
f "" _ = ""
f _ "" = ""
f (x:xs) (y:ys)
    | x == y = x : f xs ys
    | otherwise = ""


calculeaza :: [Int] -> [Int] -> Int
calculeaza xs ys = if length xs == length ys
    then sum[x ^ 2 * y ^ 2 | (x,y) <- zip xs ys]
    else error "Lungimi diferite"