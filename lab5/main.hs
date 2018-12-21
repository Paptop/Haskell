import Text.Printf

-- Ex 2 --
data NumList = Nlist [Float]
    deriving (Show)
    
average :: NumList -> Float
average (Nlist []) = 0
average (Nlist list) = (sum list) / fromIntegral( length list )

instance Eq NumList where
    a == b = (average a) == (average b)

instance Ord NumList where
    a <= b = (average a) <= (average b)
    
test_list_0 = do
    let list0 = Nlist [1,2,2,1]
    let list1 = Nlist [3,1,1,1]
    print list0
    print list1
    print (list0 == list1)

-- Ex 3 --
instance (Num a, Num b) => Num (a -> b) where
    f0 + f1 = \arg -> f0 arg + f1 arg
    f0  * f1 = \arg -> f0 arg  * f1 arg
    negate f = negate . f
    fromInteger n = \x -> fromInteger n
    abs f = abs . f
    signum f = signum . f
    
test_num_0 = do
    let res = ( (+1) * (+2) + 10 ) 1
    print res

-- Ex 4 --
data GTree a = Leaf a | Gnode [GTree a]
  deriving (Show)

depth :: GTree a -> Int
depth (Leaf a) = 0 
depth (Gnode leafs) = 1 + maximum (map depth leafs)

occurs :: Eq a => a -> GTree a -> Bool
occurs a (Leaf la) = (a == la)
occurs a (Gnode leafs) = foldr (||) False [occurs a b | b <- leafs]

mapTree :: (a -> a) -> GTree a -> GTree a
mapTree f (Leaf a) = Leaf ( f a )
mapTree f (Gnode leafs) = Gnode( map  (mapTree f) leafs)

test_tree_0 = do
    let tree = Gnode [Leaf 1, Gnode [ Leaf 2] ]
    print tree
    let dep = depth tree
    printf "Depth of a tree %d \n" dep 
test_tree_1 = do
    let tree = Gnode [Leaf 1, Leaf 2, Gnode [ Leaf 3 ] ]
    print tree
    let isInList = occurs 3 tree
    printf "Element 3 occurs: "
    print isInList
    let dep = depth tree
    printf "Depth of a tree %d \n" dep 
    
test_tree_2 = do
    let tree = Gnode [Leaf 1, Leaf 2, Gnode [ Leaf 3] ]
    print tree
    let isInList = occurs 4 tree
    printf "Element 4 occurs: "  
    print isInList
    let dep = depth tree
    printf "Depth of a tree %d \n" dep 
    
test_tree_3 = do
    let tree = Gnode [Gnode [Leaf 3, Gnode [ Leaf 4] ], Leaf 2,  Leaf 1 ]
    print tree
    let isInList = occurs 4 tree
    printf "Element 4 occurs: " 
    print isInList
    let dep = depth tree
    printf "Depth of a tree %d \n" dep
    
test_tree_4 = do
    let tree = Gnode [Gnode [Leaf 3],  Leaf 2,  Leaf 1 ]
    print tree
    let isInList = occurs 3 tree
    printf "Element 3 occurs: "
    print isInList
    let dep = depth tree
    printf "Depth of a tree %d \n" dep
test_tree_5 = do
    let tree = Gnode [Gnode [Leaf 3], Gnode [Leaf 4, Gnode [Leaf 5], Leaf 6, Leaf 7, Gnode[ Leaf 10]  ], Gnode [Leaf 1]]
    print tree
    let dep = depth tree
    printf "Depth of a tree %d \n" dep 
    let isInList = occurs 10 tree
    printf "Element 10 occurs: "
    print isInList
    let newTree = mapTree (+1) tree
    print newTree
    
-- Ex 5--
data Result a = OK a | Error String
    deriving (Show)
    
error0 :: Float -> Result Float
error0 val
    | val < 0 = Error "Value must be positive"
    | otherwise = OK val
    
error1 :: Float -> Float -> Result Float
error1 b val
    | val > b = Error "Value exccedes boundaries"
    | otherwise = OK val
    
composeResult f g arg =
    case (f arg) of
         Error str -> (f arg)
         OK val -> g val
         
compose_test = do
    let com = composeResult error0 (error1 10)
    print (com 5)
    print (com (2 ^ 10) )
    print (com (1 - 10)  )
    
-- Ex 6--
sub_sum :: [Integer] -> [Integer] -> Integer -> [(Integer,Integer)]
sub_sum a b t =  [ (x,y) | x <- a , y <- b, x + y == t]

gold :: Integer -> [(Integer,Integer)]
gold b  = sub_sum (primes b) (primes (b `div` 2)) b

golden :: Integer -> Bool
golden = not.null.gold

goldbach :: Integer -> [Bool]
goldbach b = [ golden y | y <- [4,6..b] ]

primes :: Integer ->  [Integer]
primes b = sieve [2..b]

sieve [] = []
sieve (x:xs) = x : sieve [ y | y <-xs, y `mod` x > 0]

-- Ex 7 --
data Stream a = Cons a (Stream a)
  deriving(Show)
  
stream_to_list :: Stream a -> [a]
stream_to_list (Cons x xs) = x : (stream_to_list xs)

stream_iter :: (a -> a) -> a -> Stream a
stream_iter f x = Cons x (stream_iter f (f x) )

stream_iter_leave :: Stream a -> Stream a -> Stream a
stream_iter_leave (Cons x xs) ys = Cons x (stream_iter_leave ys xs)

stream_test = do
      let evenStream = stream_iter (\x -> x * 2) (1)
      let negStream = stream_iter (\x -> x - 1) (-1)
      let  res = Cons 0 (stream_iter_leave evenStream negStream)
      let  res_list = stream_to_list res
      print res_list

-- Ex 1 --
data Valuation a = Valuation {name::Var, value::a}
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [Expr a] -> a
type Var = String

sub :: [Valuation a] -> Var -> a 
sub val target
               | length(val) == 0 = error ("Target not present" ++ target)
               | name (head val) == target = value (head val)
               | otherwise = sub (tail val) target

eval :: Expr a -> a
eval  (Lit a) = a
eval  (EVar a) = error ("Not bound "++a)
eval  (Op f list) = f list

simplify :: [Valuation a] -> Expr a -> Expr a
simplify val expr = 
  case expr of 
    Lit a -> Lit a
    EVar n -> Lit (sub val n)
    Op f exs -> Op f (foldr(\ex acc -> (simplify val ex) : acc ) [] exs)

eval_final val ex = eval (simplify val ex)

eval_test = do 
    -- 1 + 1, 2 + 1,  10 + 1
    let mult expr = (foldr (\x a -> a * (eval x) ) 1 expr) 
    let expList = [Lit 1, Lit 2, EVar "a"]
    -- 1 * 2 * a
    let e1 = Op mult expList
    let v1 = Valuation "a" 10
    let v2 = Valuation "b" 7
    -- 1 * 2 * a(10) * 10 * b(7)
    let fullExp = Op mult [e1, Lit 10, EVar "b"]
    let res = eval_final [v1,v2] fullExp
    print res
