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

(* ----- Solutions | Name Substitutions ----- *)
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
        | strs::strss =>
            let
                val test = all_except_option(str, strs)
            in
                case test of
                NONE => get_substitutions1(strss, str)
                | SOME strs => strs @ get_substitutions1(strss, str)
            end

(* Problem 3 *)
fun get_substitutions2(strss, str) =
    let
        fun helper_tail(accu, strss, str) =
            case strss of
            [] => accu
            | strs::strss =>
                let
                    val test = all_except_option(str, strs)
                in
                    case test of
                    NONE => helper_tail(accu, strss, str)
                    | SOME strs => helper_tail(rev(strs) @ accu, strss, str)
                    (* reversed accu to avoid O(n^2) copying *)
                end
    in
        rev(helper_tail([], strss, str))    (* Reverse accu *)
    end

(* Problem 4 *)
fun similar_names(strss, name) =
    let
        val {first = first, middle = middle, last = last} = name
        val firstNames = get_substitutions2(strss, first)
        fun helper_tail(accu, firstNames) =
            case firstNames of
            [] => accu
            | (newFirst::firstNames) => 
                let
                    val newName = {first = newFirst, middle = middle, last = last}
                in
                    helper_tail(newName::accu, firstNames)
                end
    in
        rev(helper_tail([name], firstNames))
    end

(* ----- Solutions | Solitaire Card Game ----- *)
(* Problem 5 *)
fun card_color(card) =
    let
        val (suit, _) = card
    in
        case suit of
        Spades => Black
        | Clubs => Black
        | Diamonds => Red
        | Hearts => Red
    end

(* Problem 6 *)
fun card_value(card) =
    let
        val (_, rank) = card
    in
        case rank of
        Num n => n
        | Ace => 11
        | _ => 10
    end

(* Problem 7 *)
fun remove_card(cs, c, e) =
    let
        fun remove_card_tail(cs, accu) =
            case cs of
            [] => raise e   (* Card not found *)
            | top::cs => 
                if c = top
                then rev(accu) @ cs
                else remove_card_tail(cs, top::accu)    (* Tail Recursion *)
    in
        remove_card_tail(cs, [])
    end

(* Problem 8 *)
fun all_same_color(cs) =
    let
        fun all_same_color_tail(cs, color) =
            case cs of
                [] => true
                | c::cs =>
                    if card_color(c) = color
                    then all_same_color_tail(cs, color)
                    else false
    in
        case cs of
        [] => true
        | c::cs => all_same_color_tail(cs, card_color(c))
    end
        
(* Problem 9 *)
fun sum_cards(cs) =
    let
        fun sum_cards_tail(cs, accu) =
        case cs of
        [] => accu
        | c::cs => sum_cards_tail(cs, accu + card_value(c))
    in
        sum_cards_tail(cs, 0)
    end


(* Problem 10 *)
fun score(cs, goal) = 
    let
        val score_raw = sum_cards(cs) - goal
        val preliminary_score = 
            if score_raw > 0
            then 3 * score_raw
            else ~score_raw
    in
        if all_same_color(cs)
        then preliminary_score div 2
        else preliminary_score
    end


(* Problem 11 *)
fun officiate(cs, moves, goal) =
    let
        fun continue(cs, moves, helds) =
            case moves of
            [] => helds
            | (Discard c)::moves => 
                continue(cs, moves, remove_card(helds, c, IllegalMove))
            | Draw::moves =>
                case cs of
                [] => helds
                | c::cs => 
                    if sum_cards(c::helds) >= goal
                    then c::helds
                    else continue(cs, moves, c::helds)
    in
        score(continue(cs, moves, []), goal)
    end


(* Problem 12 *)
fun score_challenge(cs, goal) =
    let
        fun card_value_challenge(card, score) =
            let
                val (_, rank) = card
            in
                case rank of
                Num n => n
                | Ace => score
                | _ => 10
            end
        fun sum_cards_challenge(cs, score) =
            let
                fun sum_cards_tail(cs, accu) =
                case cs of
                [] => accu
                | c::cs => sum_cards_tail(cs, accu + card_value_challenge(c, score))
            in
                sum_cards_tail(cs, 0)
            end
        fun score_challenge_helper(cs, goal, score) = 
            let
                val score_raw = sum_cards_challenge(cs, score) - goal
                val preliminary_score = 
                    if score_raw > 0
                    then 3 * score_raw
                    else ~score_raw
            in
                if all_same_color(cs)
                then preliminary_score div 2
                else preliminary_score
            end
    in
        Int.min(score_challenge_helper(cs, goal, 11),
            score_challenge_helper(cs, goal, 1))
    end

fun officiate_challenge(cs, moves, goal) =
    let
        fun continue_challenge(cs, moves, helds) =
            case moves of
            [] => helds
            | (Discard c)::moves => 
                continue_challenge(cs, moves, remove_card(helds, c, IllegalMove))
            | Draw::moves =>
                case cs of
                [] => helds
                | c::cs => 
                    if sum_cards(c::helds) >= goal
                    then c::helds
                    else continue_challenge(cs, moves, c::helds)
    in
        score(continue_challenge(cs, moves, []), goal)
    end


(* Problem 13 *)
fun careful_player(cs, goal) =
    let
        fun max_card(cs) =
            case cs of
            [] => (Clubs, Num 0)
            | c::cs => 
                let
                    val candidate = max_card(cs)
                in
                    if card_value(c) > card_value(candidate)
                    then c
                    else candidate
                end
                
        fun helper(cs, holds) =
            case cs of
            [] => []
            | c::cs =>
                if sum_cards(c::holds) = goal 
                then []
                else if (goal - sum_cards(holds) > 10) 
                    orelse (sum_cards(c::holds) < goal)
                then Draw::helper(remove_card(cs, c, IllegalMove), c::holds)
                else
                    let
                        val maxCard = max_card(holds)
                    in
                        if card_value(c) >= card_value(maxCard)  (* Not possible *)
                        then []
                        else Draw::((Discard maxCard)
                            ::helper(remove_card(cs, c, IllegalMove), 
                                c::(remove_card(holds, maxCard, IllegalMove))))
                    end      
    in  
        helper(cs, [])
    end