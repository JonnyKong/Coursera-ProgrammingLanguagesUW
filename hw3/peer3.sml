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


(* 1. *)
fun only_capitals strs =
  List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs
(* 5 pts *)

(* 2. *)
fun longest_string1 strs =
  List.foldl (fn (a,b) => if String.size a > String.size b then a else b) "" strs
(* 5 pts *)

(* 3. *)
fun longest_string2 strs =
  List.foldl (fn (a,b) => if String.size a >= String.size b then a else b) "" strs
(* 5 pts *)

(* 4a. *)
fun longest_string_helper f strs =
  List.foldl (fn (a,b) => if f(String.size a, String.size b) then a else b) "" strs


(* 4b. *)
val longest_string3 =
  longest_string_helper (fn (a,b) => a>b)


(* 4c. *)
val longest_string4 =
  longest_string_helper (fn (a,b) => a>=b)
(* 5 pts *)

(* 5. *)
val longest_capitalized =
  longest_string3 o only_capitals
(* 5 pts *)

(* 6. *)
val rev_string =
  String.implode o List.rev o String.explode
(* 5 pts *)


(* 7. *)
fun first_answer f xs =
  case xs
    of [] => raise NoAnswer
     | x::xs' => case (f x)
                   of NONE => first_answer f xs'
                    | SOME v => v
(* 5 pts *)


(* 8. *)
fun all_answers f xs =
  let
    fun helper acc partial_xs =
      case partial_xs
        of [] => SOME acc
         | p_x::p_xs' => case (f p_x)
                           of NONE => NONE
                            | SOME v => helper (acc @ v) p_xs'
  in
    helper [] xs
  end
(* 5 pts *)

(* 9a. *)
val count_wildcards =
  g (fn x => 1) (fn x => 0)


(* 9b. *)
val count_wild_and_variable_lengths =
  g (fn x => 1) String.size


(* 9c. *)
fun count_some_var (s,p) =
  g (fn x => 0) (fn x => if x=s then 1 else 0) p
(* 5 pts *)


(* 10. *)
val check_pat =
  let
    fun get_all_vars p =
      case p
        of Variable x => [x]
         | TupleP ps => List.foldl (fn (p, acc_vars) => (get_all_vars p) @ acc_vars) [] ps
         | _ => []

    fun has_repeats vs =
      case vs
        of [] => false
         | v::vs' => List.exists (fn x => x=v) vs' orelse has_repeats vs'
  in
    not o has_repeats o get_all_vars
  end
(* 5 pts *)


(* 11. *)
fun match (va, pa) =
  case (va, pa)
    of (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s,v)]
     | (Unit, UnitP) => SOME []
     | (Const x, ConstP y) => if x=y then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if length vs = length ps
                                then let val x = ListPair.zip(vs,ps) in all_answers match x end
                                else NONE
     | (Constructor (s,v), ConstructorP (s',p)) => if s=s' then match(v,p) else NONE
     | _ => NONE
(* 5 pts *)

(* 12. *)
fun first_match v ps =
  SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
(* 5 pts *)

(*
use "/home/swatson/Dropbox/coursera/programming-languages/Week-3/hw3.sml";
use "/home/swatson/Dropbox/coursera/programming-languages/Week-3/hw3test.sml";
*)

