module Curs1 where
import Data.List

primes = sieve [2..]
sieve(p : ps) = p : sieve [x | x <- ps, mod x p /= 0]


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort(p:xs) = 
    (qsort lesser) ++ [p] ++ (qsort greater)
    where
        lesser = filter(< p) xs
        greater = filter(> p) xs


x = let z = 5; g u = z + u in let z = 7 in g 0

--f x y = x ** 2 + y ** 2

parImpar x = if x `mod` 2 == 0 then "par" else "impar"

fac x = if x == 0 then 1 else x * fac(x - 1)

maiMare x y = if x > 2 * y then "Da" else "Nu"

maxim x y = if x > y
    then x 
    else y

maxim3 x y z = let u = maxim x y
    in maxim u z

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact(x - 1)

semn :: Integer -> Integer
semn 0 = 0
semn x
    | x > 0     = 1
    | otherwise = -1


f x = let x = 3 ; y = 4 in x + y