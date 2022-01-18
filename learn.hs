module Learn where
import Data.List
import System.IO
import Distribution.Simple.PackageIndex (reverseTopologicalOrder)
import Text.Parsec.Char (newline)


-- Int -2^63 2^63

maxInt = maxBound :: Int


minInt = minBound :: Int


bigFloat = 3.99999999999999 + 0.00000000000005

--suma elem din lista
suma = sum [1..1000]

--modalitati de a scrie 5 % 4
modex = mod 5 4
modex2 = 5 `mod` 4

num9 = 9 :: Int 

--radical
sqrt0f9 = sqrt(fromIntegral num9)

primenr = [3, 5, 7, 11, 13]

--concatenare a 2 liste cu '++'
moreprimes = primenr ++ [17, 19, 23, 29, 31, 37]

favnr = 2:7:21:[]

double :: Integer -> Integer 
double x = x * 2

maxim :: Integer -> Integer -> Integer 
maxim x y = if(x > y) then x else y

maxim3 :: Integer -> Integer -> Integer -> Integer 
maxim3 x y z = let u = maxim x y in maxim u z

newlist = [0..10]

--lista nr pare
evenlist = [0,2..100]

--ia primele 10 elem din lista formata repetand 2
multi = take 10(repeat 2)

--lista de 10 elem repetand '3'
multi3 = replicate 10 3

--creaza un ciclu de 10 elemente din lista data repetandu le
ciclu = take 10 (cycle[1,2,3])

--conditie de creare a listei
li = [x * 2 |  x <- [1..10]]

div_cu_9_si_13 = [x | x <- [1..500], x `mod` 9 == 0, x `mod` 13 == 0]

--sortare 
sortata = sort[1, 9, 2, 3, 8, 10]

--zipWith aduna in a 2 a lista elem din prima
sumadeliste = zipWith (+) [1, 2,3,4,5][6,7,8,9]


--filter  ia mai mult decat 5 elem
listamaimarede5 = filter(>5) moreprimes


--takeWhile ia elementele mai mici sau egale cu 20
evensupto20 = takeWhile (<= 20) ([2,4..])


--foldl sau foldr  --inmulteste de la stanga(foldl) la dreapta sau invers
multipleList = foldl (*) 1 [2,3,4,5]

--foldl sau foldr  --inmulteste de la stanga(foldl) la dreapta sau invers
multipleList2 = foldl (+) 0 [2,3,4,5]

--list comprehension

pow3List = [3 ^ n | n <- [1..10]]

--matrice
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

--tupluri

randTuple = (1, "Random Tuple")

bobSmith = ("Bob", 52)

bobsName = fst bobSmith

bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom "]

addresses = ["123 Main", "234 North", "567 South"]

namesNAddress = zip names addresses


--functii

main = do
    putStrLn "what's your name"
    name <- getLine 
    putStrLn ("hello " ++ name)

