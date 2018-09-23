-- Welcome to first haskel program

size::Int
size=12+13

square::Int->Int
square x=x*x

double::Int->Int
double x=2*x

func::Int->Int
func =square.double

evaluate::Int->Int->Bool
evaluate x y = x == y

example::Int
example=double(size-square(2*2))
