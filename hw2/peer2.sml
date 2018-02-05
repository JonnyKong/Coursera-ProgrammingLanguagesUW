(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun reverse xs =
    let fun aux(xs,acc) =
            case xs of
                [] => acc
              | x::xs' => aux(xs', x::acc)
    in
        aux(xs,[])
    end

fun all_except_option (s, ls) =
    let
        fun go(l, contains, acc) =
            case l of
                  [] => if contains then SOME (reverse acc) else NONE
                | x :: xs => if same_string(s, x) then go(xs, true, acc) else go(xs, contains, x :: acc)
    in
        go (ls, false, [])
    end
(* 5 pts *)

fun get_substitutions1 (l, s) =
    case l of
          [] => []
        | x :: xs => let val optL = all_except_option (s, x)
                     in
                        case optL of
                              NONE => get_substitutions1(xs, s)
                            | SOME ls => ls @ get_substitutions1(xs, s)
                     end
(* 5 pts *)

fun get_substitutions2 (l, s) =
    let
        fun go(ls, acc) =
            case ls of
                  [] => acc
                | x :: xs => let val optL = all_except_option (s, x)
                             in
                                 case optL of
                                     NONE => go(xs, acc)
                                     | SOME ls => go(xs, acc @ ls)
                             end
    in
        go (l, [])
    end
(* 5 pts *)

type FullName = { first:string, middle:string, last:string }

fun similar_names (l, name) =
    let
        val {first=x,middle=y,last=z} = name

        fun mapToFullName ls =
            case ls of
                  [] => []
                | firstN :: xs => {first=firstN, middle=y, last=z} :: mapToFullName xs
    in
        name :: mapToFullName (get_substitutions1 (l, x))
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

fun card_color c =
    case c of
          (Clubs , _) => Black
        | (Spades, _) => Black
        | _ => Red
(* 5 pts *)

fun card_value c =
    case c of
          (_, Num i) => i
        | (_, Ace) => 11
        | _ => 10
(* 5 pts *)

fun remove_card (cs, c, e) =
    case cs of
          [] => raise e
        | x :: xs => if x = c then xs else x :: remove_card (xs, c, e)
(* 5 pts *)

fun all_same_color cs =
    case cs of
          [] => true
        | _ :: [] => true
        | c1 :: c2 :: xs => if card_color(c1) = card_color c2
                                      then all_same_color(c2 :: xs)
                                      else false
(* 5 pts *)

fun sum_cards cs =
    let
        fun go (cards, sum) =
            case cards of
                  [] => sum
                | x :: xs => go (xs, sum + card_value x)
    in
        go (cs, 0)
    end
(* 5 pts *)

fun score (cs, goal) =
    let
        val sum = sum_cards cs
        val prel_score = if sum > goal then 3 * (sum - goal) else goal - sum
        val final_score = if all_same_color cs then prel_score div 2 else prel_score
    in
        final_score
    end
(* 5 pts *)

fun officiate (cs, ms, goal) =
    let
        fun go (cards, moves, hand) =
            case (cards, moves) of
                  (_, []) => score (hand, goal)
                | ([], Draw :: _) => score (hand, goal)
                | (_, Discard c :: ys) => go (cards, ys, remove_card (hand, c, IllegalMove))
                | (c :: xs, Draw :: ys) => go (xs, ys, c :: hand)
    in
        go (cs, ms, [])
    end
(* 5 pts *)
 
fun score_challenge (cs, goal) =
    let
        fun number_of_aces cards =
            case cards of
                  [] => 0
                | (_, Ace) :: xs => 1 + number_of_aces xs
                | _ :: xs => number_of_aces xs

        val aces = number_of_aces cs
        val prel_sum = sum_cards cs
        val sum = if (prel_sum > goal)
                  then prel_sum - Int.min((prel_sum - goal - 3) div 10 + 1, aces) * 10
                  else prel_sum
        val prel_score = if sum > goal then 3 * (sum - goal) else goal - sum
        val final_score = if all_same_color cs then prel_score div 2 else prel_score
    in
        final_score
    end


fun officiate_challenge (cs, ms, goal) =
    let
        fun go (cards, moves, hand) =
            case (cards, moves) of
                  (_, []) => score_challenge (hand, goal)
                | ([], Draw :: _) => score_challenge (hand, goal)
                | (_, Discard c :: ys) => go (cards, ys, remove_card (hand, c, IllegalMove))
                | (c :: xs, Draw :: ys) => go (xs, ys, c :: hand)
    in
        go (cs, ms, [])
    end

fun careful_player (cs, goal) =
    let
        fun remove_card_for_play (cards_in_hand, sum, v, new_hand, removed_card) =
            case (cards_in_hand, removed_card) of
                  ([], _) => (new_hand, removed_card, sum)
                | (x :: xs, SOME c) =>
                    let
                        val new_sum = sum - card_value x + v
                    in
                        if new_sum = goal
                        then (new_hand @ xs, SOME x, new_sum)
                        else if new_sum < goal andalso sum < new_sum
                        then remove_card_for_play (xs, new_sum, v, c :: new_hand, SOME x)
                        else remove_card_for_play (xs, sum, v, x :: new_hand, removed_card)
                    end
                | (x :: xs, NONE) =>
                    let
                        val new_sum = sum - card_value x + v
                    in
                        if new_sum = goal
                        then (new_hand @ xs, SOME x, new_sum)
                        else if new_sum < goal andalso sum < new_sum
                        then remove_card_for_play (xs, new_sum, v, new_hand, SOME x)
                        else remove_card_for_play (xs, sum, v, x :: new_hand, NONE)
                    end

        fun go (cards, plays, cards_in_hand, sum) =
            case cards of
                  [] => plays
                | x :: xs =>
                    let
                        val cur_card_value = card_value x
                        val new_sum = cur_card_value + sum
                    in
                        if new_sum = goal
                        then Draw :: plays
                        else if new_sum < goal
                        then go (xs, Draw :: plays, x :: cards_in_hand, new_sum)
                        else
                            let
                                val (new_hand, removed_card, new_sum) = remove_card_for_play (cards_in_hand, sum, cur_card_value, [], NONE)
                            in
                                case removed_card of
                                      NONE => plays
                                    | SOME x =>
                                        if new_sum = goal
                                        then Draw :: Discard x :: plays
                                        else go (xs, Draw :: Discard x :: plays, x :: cards_in_hand, new_sum)
                            end
                    end
    in
        if goal = 0 then [] else reverse (go (cs, [], [], 0))
    end