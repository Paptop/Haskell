module IJMain where

import Data.List
import Data.Char
import Data.Function (on)

-- Ex 1 --
average:: [Float] -> Float
average list = (sum list) / fromIntegral(length list) 

-- Ex 2 --
divides:: Integer -> [Integer]
divides x =  [n | n <- [1..( (abs x) `div` 2)], ( (abs x) `mod` n) == 0] ++ [ (abs x) ]

dividesRec:: [Integer] -> Integer -> [Integer]
dividesRec [] _ = [] 
dividesRec (x:xs) n 
           | n `mod` x == 0 = x:(dividesRec xs n)      
           | otherwise = dividesRec xs n

dividesRecursive::Integer -> [Integer]
dividesRecursive n = dividesRec [1..n] n

isPrime:: Integer -> Bool
isPrime p = [1,p] == (divides p)

-- Ex 3 --
prefix:: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substring:: String->String-> Bool
substring _ [] = False
substring xs ys = prefix xs ys || substring xs (tail ys)

-- Ex 4 --
permut:: [Integer] -> [Integer] -> Bool
permut x y = (sort x) == (sort y)
        where sX = sort x
              sY = sort y 

-- Ex 5 --
capitalise:: String -> String
capitalise str = [ toUpper c | c <- str, isAlpha c]

-- Ex 6 --
foo::[(String,Float)] -> (String,Float)
foo input = ( fst (head input) , sum ([ snd p | p <- input]))

itemTotal:: [(String,Float)] -> [(String,Float)]
itemTotal input  = map foo (groupBy ((==) `on` fst) (sort input))

isKey::(String,Float) -> String -> Integer -> (String,Float) 
isKey p str disc
        | str == (fst p) = (fst p, (snd p) - (snd p) * (fromIntegral disc) * 0.01)
        | str /= (fst p) = p

itemDiscount:: String -> Integer -> [(String,Float)] -> [(String,Float)]
itemDiscount key disc input = [isKey p key disc | p <- input] 
