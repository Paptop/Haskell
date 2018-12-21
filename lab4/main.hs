-- Ex 1 --
myLength :: [a] -> Integer
myLength = sum . map (\_ -> 1) 


-- Ex 2 --
removeNeg :: [Integer] -> [Integer]
removeNeg [] = []
removeNeg (x:xs)
          | x > 0 = x : (removeNeg xs)
          | otherwise = removeNeg xs

ff :: Integer -> [Integer] -> Integer
ff maxNum  = accumulate . multiply . removeNeg 
      where 
          multiply = map (*10) 
          accumulate = foldl (\a x -> if (a + x) > maxNum then a else x + a) 0   

-- Ex 3 --
flip :: (a->b->c) -> (b->a->c)
flip = \f a b -> f b a

-- Ex 4 --


mytotal2 :: (Integer -> Integer) -> Integer -> Integer
mytotal2 f n = foldr( \x a -> f x + a) 0 [0..n] 
--
mytotal :: (Integer -> Integer) -> [Integer] -> Integer
mytotal f =  sum . map f 

total :: (Integer -> Integer) -> Integer -> Integer
total f n = mytotal f [0..n] 

-- Ex 5 --

iterRec :: Int -> (a->a) -> (a->a)
iterRec n f
    | n > 0 = f . (iterRec (n-1) f)
    | otherwise = id


iter :: Int -> (a->a) -> (a->a)
iter n f
      | n >= 0 = foldr(\x acc -> x . acc) id (replicate (n) f)
      | otherwise = id

-- Ex 6 --

splits :: [a] -> [([a],[a])]
splits xs = foldr( \x acc -> (splitAt x xs) : acc) [] [0..n]
  where n = length xs

-- Ex 7 --
type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (=="")

char :: Char -> RegExp
char ch = (==[ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x

(<<*>>) :: RegExp -> RegExp -> RegExp
e1 <<*>> e2 = \x -> or [e1 y && e2 z | (y,z) <- splits x]

star :: RegExp -> RegExp
star p = epsilon ||| (p <<*>> star p)

option, plus :: RegExp -> RegExp
plus p = p ||| (p <<*>> (plus p))
option p = p  ||| epsilon   

