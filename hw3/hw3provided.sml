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
		| (SOME x)::xs => x


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
val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var(str, p) = g (fn() => 0) (fn x => if x = str then 1 else 0) p


(* Problem 10 *)
fun check_pat pat =
	let
		fun expand_variable(pat, accu) =
			case pat of
				Wildcard => accu
				| Variable s => s::accu
				| TupleP ps => List.foldl expand_variable accu ps
				| ConstructorP(_, pat) => expand_variable(pat, accu)
				| _ => accu
		fun has_repeat ss =
			case ss of
				[] => false
				| s::ss => (List.exists (fn x => x = s)) ss orelse has_repeat ss
	in
		not (has_repeat (expand_variable(pat, [])))
	end


(* Problem 11 *)
fun match(v, p) =
	case (p, v) of
		(Wildcard, _) => SOME []
		| (Variable s, v) => SOME [(s, v)]
		| (UnitP, Unit) => SOME []
		| (ConstP(p), Const(v)) => if p = v then (SOME []) else NONE
		| (TupleP(ps), Tuple(vs)) => if List.length(ps) <> List.length(vs) then NONE
			else all_answers match (ListPair.zipEq(vs, ps))  
		| (ConstructorP(s1, p), Constructor(s2, v)) => 
			if s1 = s2 then match(v, p) else NONE
		| _ => NONE


(* Problem 12 *)
fun first_match v ps =
	let 
		fun match_value p = match(v, p)
		val ans = SOME (first_answer match_value ps) handle NoAnswer => NONE 
	in ans end