module Looper exposing (Num, add, get, new, set, sub)


type Num
    = Num Int Int Int


new : Int -> Int -> Int -> Num
new n start end =
    Num n start end


add : Int -> Num -> Num
add x (Num n start end) =
    Num (start + modBy (end - start + 1) (n + x - start)) start end


sub : Int -> Num -> Num
sub x num =
    add (negate x) num


set : Int -> Num -> Num
set x (Num n start end) =
    add x (Num 0 start end)


get : Num -> Int
get (Num n start end) =
    n
