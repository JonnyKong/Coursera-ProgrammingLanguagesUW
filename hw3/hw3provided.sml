(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Problem 1 *)
fun only_capitals(strs) =
	List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs


(* Problem 2 *)
fun longest_string1(strs) =
	foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) 
		"" strs


(* Problem 3 *)
fun longest_string2(strs) =
	foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) 
		"" strs


(* Problem 4 *)
fun longest_string_helper f strs =
	foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) 
		"" strs


val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


(* Problem 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* Problem 6 *)
val rev_string = String.implode o List.rev o String.explode


(* Problem 7 *)
fun first_answer f xs =
	case ((List.filter isSome) o (List.map f)) xs of
		[] => raise NoAnswer
		| x::xs => valOf(x)


(* Problem 8 *)
fun all_answers f xs =
	let
		val tmp = List.map f xs
		fun my_concat(xs, accu) =
			case xs of
				[] => accu
				| (SOME x)::xs => my_concat(xs, accu@x)
	in
		case List.filter (fn x => isSome(x) = false) tmp of
			[] => SOME(my_concat(tmp, []))
			| _ => NONE
	end

(* Problem 9 *)





(* ----- Test Cases ----- *)
val test10 = only_capitals ["A","B","C"] = ["A","B","C"]
val test11 = only_capitals ["A","B","C","d"] = ["A","B","C"]
val test12 = only_capitals ["A","C","d","B"] = ["A","C","B"]

val test20 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["A","bc","CBZ"] = "CBZ"
val test22 = longest_string1 [] = ""
val test23 = longest_string1 ["AB","bcd","CBZ"] = "bcd"

val test30 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 ["A","bc","CBZ"] = "CBZ"
val test32 = longest_string2 [] = ""
val test33 = longest_string2 ["AB","bcd","CBZ"] = "CBZ"

val test4a0 = longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 ["A","bc","CBZ"] = "CBZ"
val test4a2 = longest_string3 [] = ""
val test4a3 = longest_string3 ["AB","bcd","CBZ"] = "bcd"

val test4b0 = longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 ["A","bc","C"] = "bc"
val test4b2 = longest_string4 ["A","bc","CBZ"] = "CBZ"
val test4b3 = longest_string4 [] = ""
val test4b4 = longest_string4 ["AB","bcd","CBZ"] = "CBZ"

val test50 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized [] = ""
val test52 = longest_capitalized ["AB","bc","C"] = "AB"
val test53 = longest_capitalized ["AB","bc","CB"] = "AB"
val test54 = longest_capitalized ["AB","bc","CBA"] = "CBA"

val test60 = rev_string "abc" = "cba"
val test61 = rev_string "" = ""
val test62 = rev_string "abcdef" = "fedcba"

val test70 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test71 = first_answer (fn x => if x > 2 then SOME x else NONE) [1,2,3,4,5] = 3
val test72 = first_answer (fn x => if x < 2 then SOME x else NONE) [1,2,3,4,5] = 1
val test73 = ((first_answer (fn x => if x > 6 then SOME x else NONE) [1,2,3,4,5] = 1;
			false) handle NoAnswer => true)

val test80 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7]
	= NONE
val test81 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2,3,4,5,6,7]
	= NONE
val test82 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,3,4,5,6,7]
	= NONE
val test83 = all_answers (fn x => if x > 0 then SOME [x] else NONE) [2,3,4,5,6,7]
	= SOME([2,3,4,5,6,7])
val test84 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,6]
	= SOME([2,4,6])