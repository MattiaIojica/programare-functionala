module Lab1 where

x = 54000

triple :: Integer -> Integer
triple x = 3 * x

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z =
    if (x > y)
        then if (x > z)
            then x
            else z
        else if (y > z)
            then y
            else z

