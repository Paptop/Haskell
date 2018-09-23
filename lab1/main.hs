module IJMain where

import Test.QuickCheck

--- Ex 1 ---
nAnd0::Bool->Bool->Bool
nAnd0 x y=not(x&&y)

nAnd1::Bool->Bool->Bool
nAnd1 False _ = True 
nAnd1 True x = not x 

nAnd2::Bool->Bool->Bool
nAnd2 True False = True;
nAnd2 False True = True;
nAnd2 True True = False;
nAnd2 False False = True; 

nAnd3::Bool->Bool->Bool
nAnd3 x y = not x || not y

--- Ex 2 ---
prop_and::Bool->Bool->Bool
prop_and x y = 
    (nAnd1 x y == nAnd0 x y) &&
    (nAnd2 x y == nAnd0 x y) &&
    (nAnd3 x y == nAnd0 x y) 

prop_and_0::Bool->Bool->Bool
prop_and_0 x y
           | x == False || y == False = True == nAnd3 x y
           | x == True = not y == nAnd3 x y

--- Ex 3 ---
nDigits::Integer->Int
nDigits x 
        | x>=0 = length(show x)
        | x<0  = length(show(abs(x)))

--- Ex 4 ---
nRoots::Float->Float->Float->Int
nRoots a b c
         | a == 0 = error "the first argument must be non-zero" 
         | b^2 > 4.0 * a * c = 2
         | b^2 == 4.0 * a * c = 1
         | b^2 < 4.0 * a * c = 0 

--- Ex 5 ---
findRoot::(Float->Float->Float)->Float->Float->Float->Float
findRoot op a b c = ((-b) `op` sqrt(d)) / (2 * a)    
         where d = b^2 - 4 * a * c

findNeededRoot::(Float->Float->Float)->Float->Float->Float->Float
findNeededRoot func a b c
            | nRoots a b c == 0 = error "no real roots"
            | otherwise = func first second 
            where first = findRoot (+) a b c
                  second = findRoot (-) a b c 

smallerRoot::Float->Float->Float->Float
smallerRoot a b c = findNeededRoot min a b c

largerRoot::Float->Float->Float->Float
largerRoot a b c = findNeededRoot max a b c

--- Ex 6 ---
power2::Integer->Integer
power2 n
       | n < 0 = 0
       | n == 1 = 2 
       | n > 0 = 2 * power2(n-1)

--- Ex 7 ---
--- m * n = m + m + m ... n times ---
mult::Integer->Integer->Integer
mult 0 n = 0
mult m 0 = 0
mult m n
       | m < n = mult n m
       | (m < 0) && (n < 0) = mult (abs(m)) (abs(n))
       | (m < 0) || (n < 0) = (-1) * mult (abs(m)) (abs(n))
       |  n == 1 = m 
       | otherwise = m + mult m (n - 1)

--- Ex 8 ---
prod::Integer->Integer->Integer
prod m n
       | m > n = error "Invalid range"
       | m == n = m
       | otherwise = m * (prod (m+1) n)

fac::Integer->Integer
fac n = prod 1 n


