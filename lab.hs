module Lab where
import Data.List


poly2 :: Double -> Double -> Double -> Double -> Double

poly2 a b c x = a * (x ** 2) + b * x + c


eeny :: Integer -> String

eeny x = if(even x && x /= 2)
    then "eeny"
    else "meeny"

fibo :: Integer -> Integer

fibo n 
    | n < 2 = n
    | otherwise = fibo(n - 1) + fibo(n - 2)


verifL :: [Int] -> Bool

verifL = even.length

takefinal :: [Int] -> Int -> [Int]
takefinal xs n= drop (length xs - n) xs

semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    |even h = h `div` 2 : t'
    |otherwise = t'
    where t' = semiPareRec t

myreplicate :: Int -> Int -> [Int]
myreplicate 0 _ = []
myreplicate a b = b:myreplicate (a-1)b


sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs)
    |odd x = x + sumImp(xs)
    |otherwise = sumImp(xs)

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs)
    |head x == 'A' = 1 + totalLen(xs)
    |otherwise = totalLen(xs)