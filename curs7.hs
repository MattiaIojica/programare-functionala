module Curs7 where
import Data.List
import Data.Char
import Data.Foldable
import GHC.Maybe


my_elem :: Eq a => a -> [a] -> Bool
my_elem x ys = or [x == y | y <- ys]


data MyData a b = MyData a b b

f :: (Eq a, Eq b) => MyData a b -> MyData a b -> Bool
f (MyData x1 y1 z1) (MyData x2 y2 z2 ) = x1 == x2 && y1 == y2 && z1 == z2



class Visible a where
    toString :: a -> String

instance Visible Char where
    toString c = [c]

-- class Show a where
--     show :: a -> String

-- instance Show Bool where
--     Show False = "False"
--     Show True = "True"

-- instance (Show a , Show b ) => Show ( a , b ) where
-- show ( x , y ) = "( " ++ show x ++ " , " ++ show y ++ " )"
    

data Point a b = Pt a b 
            deriving (Eq, Ord, Show)


--LABORATOR

--1
data Fruct = Mar {tip::String, viermi::Bool} | Portocala {tip::String, felii::Int}
listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]

--a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala x y)
  | x == "Tarocco" || x == "Moro" || x == "Sanguinello" = True
  | otherwise = False

--b)
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs)
  | ePortocalaDeSicilia x = felii x + nrFeliiSicilia xs
  | otherwise = nrFeliiSicilia xs

--c)
nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs)
  | mar x && viermi x = 1 + nrMereViermi xs
  | otherwise = nrMereViermi xs
  where
    mar (Mar _ _) = True
    mar (Portocala _ _) = False


--2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--b
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine x y) = Just y

--3
-- data Linie = L [Int]
--     deriving Show
-- data Matrice = M [Linie]
--     deriving Show

--a)
data Linie = L {elemente::[Int]}
  deriving Show
data Matrice = M {linii::[Linie]}
  deriving Show

verifica :: Matrice -> Int -> Bool
verifica mat nr = and [foldr (+) 0 (elemente x) == nr | x <- linii mat]

--b)
doarPozN :: Matrice -> Int -> Bool
doarPozN mat nr = and [length (filter (>0) (elemente x)) == nr | x <- linii mat, length (elemente x) == nr]

--c)
corect :: Matrice -> Bool
corect mat = func [length (elemente x) | x <- linii mat ]
  where
    func [] = True
    func [_] = True
    func (x:xs)
      | x == head xs = func xs
      | otherwise = False