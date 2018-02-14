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
(* #1 *)
fun only_capitals sl = List.filter (fn str => Char.isUpper(String.sub(str, 0))) sl
(* 5 pts *)

(* #2 *)
fun longest_string1 sl = 
	List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" sl
(* 5 pts *)

(* #3 *)
fun longest_string2 sl = (* Another implementation is using the List.foldr function, but here foldr is not allowed. *)
	List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" sl
(* 5 pts *)

(* #4 *)
fun longest_string_helper f sl = 
	List.foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) "" sl

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
(* 5 pts *)

(* #5 *)
val longest_capitalized = longest_string1 o only_capitals
(* 5 pts *)

(* #6 *)
val rev_string = String.implode o List.rev o String.explode
(* 5 pts *)

(* #7 *)
fun first_answer f l =
	case l of
		[] => raise NoAnswer
		| x::xs' => case f x of NONE => first_answer f xs'| SOME v => v
(* 5 pts *)

(* #8 *)
fun all_answers f l = 
	let fun helper (accu, l) = 
		case l of
			[] => SOME accu (* A better style: case accu of [] => NONE | _ => SOME accu, but somehow do not work here. *)
			| x::xs' => case f x of
							NONE => NONE
							| SOME v => helper(v @ accu, xs')
	in helper([], l)
	end
(* 5 pts *)

(* #9a *)
val count_wildcards = g (fn() => 1) (fn x => 0) 
(* 5 pts *)

(* #9b *)
val count_wild_and_variable_lengths = g (fn() => 1) String.size
(* 5 pts *)

(* #9c *)
fun count_some_var (str, pat) = g (fn() => 0) (fn x => if x = str then 1 else 0) pat
(* 5 pts *)

(* #10 *)
fun check_pat pat = 
	let fun extract_str (pat, accu) = (* Extract all strings in the pattern to a list. *)
			case pat of
			    Variable x        => x::accu
			  | TupleP ps         => List.foldl (fn (p,i) => (extract_str (p, accu) @ i)) [] ps
			  | ConstructorP(_,p) => extract_str (p, accu)
			  | _                 => accu
		fun distinct sl = (* Does the list has distinct elements. *)
			case sl of 
				[] => true
				| x::xs' => (not (List.exists (fn y => y = x) xs')) andalso distinct xs'
	in distinct(extract_str(pat, []))
	end
(* 5 pts *)

(* #11 *)
fun match (valu, pat) =
	case (valu, pat) of
		(_, Wildcard) => SOME []
		| (v, Variable s) => SOME [(s, v)]
		| (Unit, UnitP) => SOME []
		| (Const v, ConstP p) => if v = p then SOME [] else NONE
		| (Tuple vt, TupleP pt) => if List.length vt <> List.length pt
								   then NONE
								   else all_answers match (ListPair.zip(vt, pt)) (* Match function can be seen as an one argument function which take a tuple as argument. *)
		| (Constructor (_, vc), ConstructorP (_, pc)) => match(vc, pc)
		| _ => NONE
(* 5 pts *)


(* #12 *)
fun first_match valu pl = 
	SOME (first_answer (fn x => match (valu, x)) pl)
	handle NoAnswer => NONE
(* 5 pts *)
