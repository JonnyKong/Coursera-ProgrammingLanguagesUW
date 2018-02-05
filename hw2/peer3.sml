(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*a*)
fun all_except_option (str, lstr) = 
	case lstr of
		 [] 	  => NONE
		| x :: xs => if same_string (x, str) then  SOME xs
					 else case all_except_option (str, xs) of
					 		NONE => NONE 
					 	   |SOME y => SOME (x :: y)
(* 5 pts *)
					 	  
(*b*)
fun get_substitutions1 (substitutions, s) =
	case substitutions of
		[] 			=> []
	   | lx :: llxs => case all_except_option (s, lx) of
	   					 NONE => get_substitutions1 (llxs, s)
	   					|SOME y => get_substitutions1 (llxs, s) @ y
(* 5 pts *)

(*c*)
fun get_substitutions2 (substitutions, s) =
	let fun subs_help (lsub, acc) =
		case lsub of
			[] 		   => acc
		   |lx :: llxs =>  case all_except_option (s, lx) of
		   					 NONE 	 => subs_help (llxs, acc )
		   					|SOME  y => subs_help (llxs,  acc  @ y)
	in subs_help (substitutions, [])
	end
(* 5 pts *)

(*d*)
fun similar_names (substitutions, full_name : {first : string, middle : string, last : string}) = 
	let val  {first = fname, middle = mname, last = lname} = full_name
		fun sim_help (lsub, acc) =
			case lsub of
				[] 		 =>  acc
				|x :: xs => sim_help (xs, acc @ [{first = x, middle = mname, last = lname}]) 
	in sim_help (get_substitutions2 (substitutions, fname), [full_name])
	end
(* 5 pts *)


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*a*)
fun card_color (card1) =
	case card1 of
		(Diamonds, _) => Red
		|(Hearts,_)   => Red
		|(_,_) 		  => Black
(* 5 pts *)

(*b*)
fun card_value (card1) =
	case card1 of
		(_, Num x) => x
        |(_, Ace ) => 11
        |(_,_)     => 10
(* 5 pts *)

(*c*)
fun remove_card (cs, c, e) =
	let fun rc_help (lc, acc) =
		case lc of
			[] 		=> raise e
		   |x :: xs => if x = c then acc @ xs
					   else rc_help (xs, x :: acc)
	in rc_help (cs, [])
	end
(* 5 pts *)

(*d*)
fun all_same_color cs =
	case cs of
		[] 		=> true 
		|[x] 	=> true
		|x1 :: (x2 :: xs) => card_color x1 = card_color x2 andalso all_same_color (x2 :: xs)
(* 5 pts *)

(*e*)
fun sum_cards cs =
	let fun sum_help (lc, acc) =
			case lc of
				[] => acc
				|x :: xs => sum_help (xs, card_value x + acc)
	in sum_help (cs, 0)
	end
(* 5 pts *)

(*f*)
fun score (held_cards, goal) =
	let val sumMinusGoal =  sum_cards held_cards - goal
	    val preScore = if sumMinusGoal > 0 then  3 * sumMinusGoal
					   else abs sumMinusGoal
	in if all_same_color (held_cards) then preScore div 2
	   else preScore
	end
(* 5 pts *)

(*g*)
fun officiate (card_list, move_list, goal) = (* returns score at the end of the game*)
	let fun play (heldCards, clist, moveList) =
		case moveList of
			[] => score (heldCards, goal)
			|Draw :: movetail => (case clist of 
									[]		=> 	score (heldCards, goal)
								   |x :: xs =>  let val hc = x :: heldCards 
									 			    val sum = sum_cards hc
								 				in 	if sum >= goal  then score (hc, goal)
								 		 		   	else play (hc, xs, movetail)
								 				end)
			|(Discard c) :: movetail => play (remove_card (heldCards, c, IllegalMove), clist, movetail) 
	in play ([], card_list, move_list)
	end
(* 5 pts *)

	(*3 Challenge*)
fun card_value_sm (card1) =
	case card1 of
		(_, Num x) => x
        |(_, Ace ) => 1
        |(_,_) 	   => 10

fun sum_cards_sm (cs) =
	let fun sum_help (lc, acc) =
			case lc of
				[] => acc
				|x :: xs => sum_help (xs, card_value_sm (x) + acc)
	in sum_help (cs, 0)
	end

fun score_challenge (held_cards, goal) =
	let val sumMinusGoal   = sum_cards held_cards - goal
		val sumMinusGoalsm = sum_cards_sm held_cards - goal
	    val preScore = if sumMinusGoal > 0 andalso sumMinusGoalsm > 0
	    			   then 3 * sumMinusGoalsm
	    			   else if sumMinusGoal < 0 andalso sumMinusGoalsm < 0 
	    			   		then  abs sumMinusGoal
	    			   		else  if sumMinusGoal > 0 andalso sumMinusGoalsm < 0 
	    			   			  then  if 3 * sumMinusGoal < abs sumMinusGoalsm
	    			   			  		then 3 * sumMinusGoal
	    			   			  		else abs sumMinusGoalsm
	    			   			  else 0
	in if all_same_color held_cards 
	   then preScore div 2
	   else preScore
	end

fun officiate_challenge (card_list, move_list, goal) = (* returns score at the end of the game*)
	let fun play (heldCards, clist, moveList) =
		case moveList of
			[] => score_challenge (heldCards, goal)
			|Draw :: movetail => (case clist of 
									[]		=> 	score_challenge (heldCards, goal)
								   |x :: xs =>  let val hc = x :: heldCards 
									 				val sum1 = sum_cards hc
									 				val sum2 = sum_cards_sm hc
								 				in 	if  sum1 = goal orelse sum2 = goal 
								 					then 0
								 					else if  sum1 < goal orelse sum2 < goal
								 						 then play (hc, xs, movetail)
								 		 		   		 else score_challenge (hc, goal)
								 				end)			
			|(Discard c) :: movetail => play (remove_card (heldCards, c, IllegalMove), clist, movetail) 
	in play ([], card_list, move_list)
	end

fun careful_player (card_list, goal) = 
	let fun isScore0 (lcards, c) = 
			let fun isScore0_help (cards) =(*NONE if cannot score 0 discarding one held carsd + c, else SOME card discarded *)
					case cards of
						[] 	    => NONE
					   |x:: []  => if sum_cards (c :: remove_card (lcards, x, IllegalMove))  = goal 
					   			   then SOME x
								   else NONE
					   |x :: xs => if sum_cards (c:: remove_card (lcards, x, IllegalMove)) = goal  
					   			   then SOME x (*score 0*)
								   else isScore0_help (xs)
			in isScore0_help (lcards)
			end
		fun player_helper (cardList , heldCards, movesacc)  =
			case cardList of
				[] => movesacc
				|fc :: cardListtl => let val sumhc = sum_cards heldCards
									 in	if sumhc = goal then  movesacc
										else if sumhc >= goal - 10 
											 then
												 let val sumhcplus = sum_cards (fc :: heldCards)
												 in	if sumhcplus <= goal 
													then player_helper (cardListtl, fc :: heldCards, movesacc @ [Draw])
													else (case isScore0 (heldCards, fc) of
															NONE =>  movesacc
									  					   |SOME x => player_helper (cardList, remove_card (heldCards, x, IllegalMove), movesacc @ [Discard x]))
												 end
											 else player_helper (cardListtl, fc :: heldCards, movesacc @ [Draw])
									 end
	in player_helper (card_list, [], [])
	end