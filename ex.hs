-- module Examen where
-- import Data.Char
-- import Data.List
-- import Data.Foldable
-- import Data.Maybe


-- data Identity a = Identity a
--     deriving (Eq,Show)



-- instance Foldable Identity where
-- 	foldMap f (Identity x) = f x


-- instance Foldable (Identity a) where
-- 	foldr f z (Identity x) = f x z


-- instance Foldable Identity where
-- 	foldr f z (Identity x) = f z x


-- instance Foldable Identity where
-- 	foldMap f z (Identity x) = f z x



-- f :: String -> [String] -> Bool
-- f "" _ = False
-- f _ [] = False
-- f s (x:xs) = if (isPrefixOf s x == True)
--                 then if(length x `mod` 2 == 0)
--                     then True
--                     else
--                         False
--                 else
--                     True


--Problema 11
-- functie ce returneaza lista divizorilor unui numar trimis ca parametru
-- divizori :: Int -> [Int]
-- divizori l = [x | x <- [1..l], l `mod` x == 0]

-- f :: [Int] -> Int -> Int -> [[Int]]
-- f l x y = [divizori i | (a, i) <- zip l [0..length l - 1], a `elem` [x..y]]
-- facem zip intre elementele listei si indexul de la 0 la length l - 1
-- pentru a obtine numarul si indexul acestuia
-- iar daca numaul se afla in intervalul [x, y]
-- apelam functia divizori pentru indexul acestuia


--Problema 8

-- f :: String -> [String] -> Bool
-- f s [] = True --daca lista este goala returnam True
-- f s (x:xs)
--     --functia predefinita isPrefixOf returneaza True sau False daca sirul este sau nu prefix al altui sir
--     |isPrefixOf s x == True && length x `mod` 2 == 0 = f s xs 
--     -- daca sirul x are lungime para si are ca prefix sirul s si trecem la urmatorul sir din lista
--     |isPrefixOf s x == True && length x `mod` 2 == 1 = False
--     -- daca situl x are lungime impara si are ca prefix sirul s atunci returnam False
--     |otherwise = f s xs
--     --in ultimul caz ramas (isPrefixOf s x == False) trecem la urmatorul sir din lista
    

--presupun ca exemplul trebuia scris invers

--mai intai sirul si apoi lista de siruri

--f “this” [“this is it”, “this is that”, “another example!”]  = True

-- import Data.Char
-- import Data.List

-- f :: String -> [String] -> Bool
-- f s [] = True --daca lista este goala returnam True
-- f s (x:xs)
--     --functia predefinita isPrefixOf returneaza True sau False daca sirul este sau nu prefix al altui sir
--     |isPrefixOf s x == True && length x `mod` 2 == 0 = f s xs
--     -- daca sirul x are lungime para si are ca prefix sirul s si trecem la urmatorul sir din lista
--     |isPrefixOf s x == True && length x `mod` 2 == 1 = False
--     -- daca situl x are lungime impara si are ca prefix sirul s atunci returnam False
--     |otherwise = f s xs
--     --in ultimul caz ramas (isPrefixOf s x == False) trecem la urmatorul sir din lista

-- --Exemple:
-- --f "this" ["this is it", "this is that", "another example!"] => True
-- --f "this" ["this is it", "this is tha", "another example!"] => False
-- --f "this" [] => True

import Data.Char
import Data.List

--functie ce returneaza lista divizorilor unui numar trimis ca parametru
divizori :: Int -> [Int]
divizori l = [x | x <- [1..l], l `mod` x == 0]

f :: [Int] -> Int -> Int -> [[Int]]
f l x y = [divizori i | (a, i) <- zip l [0..length l - 1], a `elem` [x..y]]
--facem zip intre elementele listei si indexul de la 0 la length l - 1
--pentru a obtine numarul si indexul acestuia
--iar daca numarul se afla in intervalul [x, y]
--apelam functia divizori pentru indexul acestuia

--Exemple:
--f [1,5,2,3,8,5,6] 2 5 => [[1],[1,2],[1,3],[1,5]]
--f [1..10] 5 25  => [[1,2,4],[1,5],[1,2,3,6],[1,7],[1,2,4,8],[1,3,9]]
-- f[] 4 5 => []



data Reteta = Li [Ingredient]
   deriving Show

data Ingredient = Ing String Int
   deriving Show


r1 = Li [Ing "faina" 500, Ing "oua" 4, Ing "zahar" 500]

r2 = Li [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 4]

r3 = Li [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 55]

instance Eq Ingredient where
    x == y = Ing x == Ing y
    x /= y = Ing x /= Ing y

