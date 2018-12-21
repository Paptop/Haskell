module IJModule where

import Data.Char

-- Ex 1 --
prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False 
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-- newsub st oldsub --
substr :: String -> String -> String -> String 
substr [] st _ = st
substr _ [] _ = [] 
substr x y z
           | x == z = y
           | prefix x y = substr x (z ++ (drop (length x) y ) ) z 
           | otherwise =  (head y) : substr x (tail y) z

-- Ex 2 --
isPalin :: String -> Bool
isPalin str = fStr == reverse fStr 
        where fStr = [ toLower c | c <- str, isAlpha c]; 


-- Ex 3 --
foo :: String -> [String]
foo s =  case dropWhile isSpace s of
                      "" -> []
                      s' -> w : words sn
                            where (w, sn) = break isSpace s'

justifyRec:: [String] -> Int -> String -> String -> String
justifyRec (word:words) size currline res 
                      |  length word > size = error("Word size exceeds limit")
                      |  length words == 0 = newRes  ++ " " ++ word 
                      |  otherwise = justifyRec words size newLine newRes 
                where
                    line = currline ++ " " ++ word
                    isLong = (length line) > size 
                    addLine = if isLong then currline ++ "\n" else "" 
                    newLine = if isLong then word else line 
                    newRes = res ++ addLine 


justify :: String -> Int -> String
justify str size = tail (justifyRec (foo str) size "" "") 



-- Ex 4 --
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving (Show, Ord, Eq)

distance x y x1 y1 = sqrt( (x - x1)^2 + (y - y1)^2)

overlaps :: Shape -> Shape -> Bool
overlaps (Rectangle x1 y1 h1 w1) (Rectangle x2 y2 h2 w2) = not (condL || condR)
    where
        -- if left side --
        condL = l1x > r2x || l2x > r1x
        -- if right side -- 
        condR = l1y < r2y || l2y < r1y
        l1x = x1 - w1 / 2
        l1y = y1 + h1 / 2
        r1x = x1 + w1 / 2
        r1y = y1 - h2 / 2
        l2x = x2 - w2 / 2
        l2y = y2 + h2 / 2
        r2x = x2 + w2 / 2
        r2y = y2 - h2 / 2

        
overlaps (Rectangle rx ry h w) (Circle cx cy r)
        | cond0 || cond1 = False
        | cond2 || cond3 = True
        | otherwise = cond4 
    where
        circleDistanceX = abs(cx - rx)
        circleDistanceY = abs(cy - ry) 
        cornerDistance = (circleDistanceX - w/2)^2 + (circleDistanceY - h/2)^2

        cond0 = circleDistanceX > ( w / 2 + r)
        cond1 = circleDistanceY > ( h / 2 + r)

        cond2 = circleDistanceX <= ( w / 2)
        cond3 = circleDistanceY <= ( h / 2)

        cond4 = (cornerDistance <= (r^2))

        

overlaps (Circle x0 y0 r0) (Rectangle x1 y1 h w) = overlaps (Rectangle x1 y1 h w) (Circle x0 y0 r0)

overlaps (Circle x0 y0 r0) (Circle x1 y1 r1) = distance x0 y0 x1 y1 < r0 + r1

-- Ex 5 --
myAnyEazy :: (a->Bool) -> [a] -> Bool
myAnyEazy f xs = length (filter f xs ) > 0

myAllEazy :: (a->Bool) -> [a] -> Bool
myAllEazy f xs = length (filter f xs) == length xs

boolean :: Bool -> Int
boolean b
        | b == True = 1
        | b == False = 0  

condCount cond xs = total
    where 
        allResults = map (cond) xs
        total = foldr(\x n -> n + (boolean x)) 0 allResults
          
myAll :: (a->Bool) -> [a] -> Bool
myAll f xs = condCount f xs == length xs

myAny :: (a->Bool) -> [a] -> Bool
myAny f xs = condCount f xs > 0



-- Ex 6 --
myUnzip xs = foldr f ([],[]) xs
  where f = \(a,b) (as,bs) -> (a:as, b:bs)
