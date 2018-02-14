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

fun isCapital s = Char.isUpper(String.sub(s,0))    
fun only_capitals(xs:string list) = List.filter isCapital xs
(* 4 pts
Could have used anonymous functions, which are more concise. *)


fun longest_string1(xs:string list) = foldl(fn (x, acc) => if String.size(x) >  String.size(acc) then x else acc) "" xs
(* 5 pts *)

fun longest_string2(xs:string list) = foldl(fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) "" xs
(* 5 pts *)

fun longest_string_helper f xs =
  foldl(fn (x, acc) => if f(String.size(x), String.size(acc)) then x else acc) "" xs

  
val longest_string3 = fn xs => longest_string_helper (fn (a,b) => a > b)  xs 
(* 5 pts *)
val longest_string4 = fn xs => longest_string_helper (fn (a,b) => a >= b) xs 
(* 5 pts *)

val longest_capitalized = longest_string1 o only_capitals


fun rev_string(s:string) = (implode o rev o explode) s 
(* 5 pts *)

fun first_answer f xs =
    case xs of [] => raise NoAnswer
    | x::tl => case f x  of
                    SOME i => i
                |   NONE  => first_answer f tl
(* 5 pts *)
                   
fun all_answers f xs =
    let fun aux(xs, acc) =  
        case xs of [] => SOME acc
        | x::tl => case f x of
                        SOME i => aux(tl,acc@i)
                    |   NONE   => NONE
    in
        aux(xs, [])
    end
(* 5 pts *)

fun count_wildcards(p) =  g (fn () => 1) (fn x => 0) p
(* 5 pts *)

fun count_wild_and_variable_lengths(p) = g (fn () => 1) (fn x => String.size x) p

fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat(p) = 
    let 
    fun list_vars (Variable x) = [x]
    	|   list_vars (TupleP ps) = List.foldl (fn (p', acc) => acc @ list_vars(p')) [] ps
    	|   list_vars (_) = []
	fun has_repeats ([]) = false
	    |   has_repeats (x::xs) = List.exists (fn x' => x = x') xs orelse has_repeats xs
    in
	    (not o has_repeats o list_vars) p
    end
(* 3 pts
Wrong output when parsing ConstructorP *)

fun match(v, p) = 

    case (p, v) of
	    (Wildcard, _) => SOME []
        |   (Variable s, _) => SOME [(s,v)]
        |   (UnitP, Unit) => SOME []
        |   (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
        |   (TupleP ps, Tuple vs) => if List.length ps = List.length vs 
				 then all_answers (fn (vs',ps') => match(vs',ps')) (ListPair.zip(vs,ps))
				 else NONE
        |   (ConstructorP(s1,pp), Constructor(s2,pv)) => if s1 = s2 then match(pv,pp) else NONE
        |   _ => NONE
(* 5 pts *)

fun first_match v ps =  ( SOME(first_answer (fn p => match(v,p)) ps) ) handle NoAnswer => NONE
(* 5 pts *)




