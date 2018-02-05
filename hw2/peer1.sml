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

	     
(* Manu Dhaliwal, Coursera Programming Languages Part 2, Homework 2 *)

(* Problem 1 *)
(* Part a *)

(* string * string list -> string list option *)
fun all_except_option (str, strList) =
    case strList of
	[] => NONE
      | head::tail => if same_string(head, str) then SOME(tail)
		      else
			  case all_except_option(str, tail) of
			      NONE => NONE
			    | SOME list => SOME(head::list)
(* 5 pts *)
					       

(* Part b *)
(* string list list * string -> string list *)
fun get_substitutions1 (substitutions, s) =
    case substitutions of
	[] => []
      | head::tail  => case all_except_option(s, head) of
			   NONE => [] @ get_substitutions1(tail, s)
			 | SOME list => list @ get_substitutions1(tail, s) 
(* 5 pts *)


(* Part c *)

fun get_substitutions2 (substitutions, s) =
    let
	fun aux(substitutions, acc) =
	    case substitutions of
		[] => acc
	     | head::tail  =>  case all_except_option(s, head) of
				   NONE => aux(tail, acc)
				 | SOME list => aux(tail, acc@list) 
	val acc = [];
    in
	aux (substitutions, acc)
    end
(* 5 pts *)


(* Part d *)
(* string list list * {first:string, middle:string, last:string} -> {first:string, middle:string, last:string} list *)
fun similar_names (substitutions, {first= first_name, middle= middle_name, last= last_name}) =
    let
	val names = get_substitutions1(substitutions, first_name);
	fun aux(names) =
	    case names of
		[] => []
	       | head::tail => {first= head, middle= middle_name, last= last_name}::aux(tail) 
    in
	{first=first_name, middle=middle_name, last=last_name}::aux(names)
    end
(* 5 pts *)


(* Problem 2 *)
(* Part a *)

fun card_color (suit, _) =
    case suit of
	Spades => Black 
       | Clubs => Black 
       | Diamonds => Red
       | Hearts => Red  
(* 5 pts *)


(* Part b *)
(* numbered cards have their
   number as the value, aces are 11, everything else is 10 *)
fun card_value (_, rank) =
    case rank of
	Jack => 10
     | Queen => 10
     | King  => 10
     | Ace => 11
     | Num(i) => i 
(* 5 pts *)


(* Part c *)
(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
   list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
   If c is not in the list, raise the exception e. You can compare cards with =.*)	   
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | one::[] => if one = c then [] else raise e
      | one::rest => if one = c then rest else one::remove_card(rest, c, e) 
(* 5 pts *)
		     

(* Part d *)
fun all_same_color2 (cs) =
    case cs of
	[] => true
      | head::[] => true
      | first::second::[] => card_color (first) = card_color (second)
      | head::neck::tail => (card_color (head) = card_color (neck)) andalso all_same_color (neck::tail)
(* 5 pts *)

(* Part e *)
fun sum_cards (cs) =
    let
	fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | one::rest => aux(rest, acc + card_value(one))
	val acc = 0
    in
	aux(cs, acc)
    end
(* 5 pts *)

    
(* Part f *)
fun score (card_list, goal) =
    let
	val card_values = sum_cards(card_list);
	val preliminary_score = if (card_values > goal) then 3 * (card_values - goal)
				else (goal - card_values);  
    in
	if all_same_color (card_list) then preliminary_score div 2 else preliminary_score
    end
(* 5 pts *)


(* Part g *)
(* datatype move = Discard of card | Draw *)
fun officiate (card_list, move_list, goal) =
    let
	val held_cards = []
	fun play_game (card_list, move_list, held_cards) =
	    case move_list of
		[] => score(held_cards, goal)
	      | (Discard (card))::rest_of_moves => play_game (card_list, rest_of_moves, remove_card (held_cards, card, IllegalMove))
	      | (Draw)::rest_of_moves => case card_list of
					     [] => score (held_cards, goal) 
					   | head::tail => if sum_cards (head::held_cards) > goal then score (head::held_cards, goal)
							   else play_game (tail, rest_of_moves, head::held_cards)
    in
	play_game (card_list, move_list, held_cards)
    end
(* 5 pts *)