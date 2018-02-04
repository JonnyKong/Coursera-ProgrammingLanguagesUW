(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem 1 *)
fun all_except_option(p, strs) =
    let
        fun all_except_tail(p, accu, strs) =
            case strs of
                [] => raise IllegalMove
                | str::strs =>
                    if same_string(p, str)
                    then rev(accu) @ strs   (* Reverse Accumulator *)
                    else all_except_tail(p, str::accu, strs) (* Tail recursion *)
        val result = SOME (all_except_tail(p, [], strs))
            handle IllegalMove => NONE
    in
        result
    end

(* Problem 2 *)
fun get_substitutions1(strss, str) =
    case strss of
        [] => []
        | str::strs =>
            

(* ----- Test Cases ----- *)
val test10 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option ("string", ["string2"]) = NONE
val test12 = all_except_option ("string", ["string", "string2"]) 
    = SOME ["string2"]
val test13 = all_except_option ("string", ["string", "string2", "string3"]) 
    = SOME ["string2", "string3"]
val test14 = all_except_option ("string3", ["string", "string2", "string3"]) 
    = SOME ["string", "string2"]
val test15 = all_except_option ("string2", ["string", "string2", "string3"]) 
    = SOME ["string", "string3"]
val test16 = all_except_option ("string0", ["string", "string2", "string3"]) 
    = NONE