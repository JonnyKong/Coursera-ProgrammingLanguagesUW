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

val test20 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test21 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],
        ["Freddie","Fred","F"]], "Fred") 
    = ["Fredrick","Freddie","F"]
val test22 = get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],
        ["Geoff","Jeff","Jeffrey"]], "Jeff") 
    = ["Jeffrey","Geoff","Jeffrey"]
val test23 = get_substitutions1 ([["a", "b", "c"], ["a", "b", "c"], ["b", "c"]], "a") 
    = ["b", "c", "b", "c"]

val test30 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test31 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],
        ["Freddie","Fred","F"]], "Fred") 
    = ["Fredrick","Freddie","F"]
val test32 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],
        ["Geoff","Jeff","Jeffrey"]], "Jeff") 
    = ["Jeffrey","Geoff","Jeffrey"]
val test33 = get_substitutions2 ([["a", "b", "c"], ["a", "b", "c"], ["b", "c"]], "a") 
    = ["b", "c", "b", "c"]

val test40 = similar_names ([["Fred","Fredrick"],
                            ["Elizabeth","Betty"],
                            ["Freddie","Fred","F"]], 
                            {first="Fred", middle="W", last="Smith"})
        = [{first="Fred", last="Smith", middle="W"}, 
            {first="Fredrick", last="Smith", middle="W"},
            {first="Freddie", last="Smith", middle="W"}, 
            {first="F", last="Smith", middle="W"}]
val test41 = similar_names ([["Fred","Fredrick"],
                            ["Elizabeth","Betty"],
                            ["Freddie","Fre","F"]], 
                            {first="Fred", middle="W", last="Smith"})
	    = [{first="Fred", last="Smith", middle="W"}, 
            {first="Fredrick", last="Smith", middle="W"}]
val test42 = similar_names([["Jonny", "Johnny"],
                            ["Jonny", "Zhaoning"],
                            ["Jonny", "Zhao Ning"],
                            ["Bahen", "Center"]],
                            {first = "Jonny", middle="", last = "Kong"})
        = [{first = "Jonny", middle="", last = "Kong"},
            {first = "Johnny", middle="", last = "Kong"},
            {first = "Zhaoning", middle="", last = "Kong"},
            {first = "Zhao Ning", middle="", last = "Kong"}]

val test50 = card_color (Clubs, Num 2) = Black
val test51 = card_color (Spades, Num 1) = Black
val test52 = card_color (Hearts, Jack) = Red
val test53 = card_color (Diamonds, King) = Red

val test60 = card_value (Clubs, Num 2) = 2
val test61 = card_value (Spades, Num 7) = 7
val test62 = card_value (Diamonds, Ace) = 11
val test63 = card_value (Hearts, King) = 10
val test64 = card_value (Clubs, Jack) = 10

val test70 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test71 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) 
    = [(Hearts, Ace)]
val test72 = remove_card ([(Hearts, Ace), (Spades, Ace)], (Hearts, Ace), IllegalMove) 
    = [(Spades, Ace)]
val test73 = ((remove_card ([(Spades, Ace)], (Hearts, Ace), IllegalMove); false) 
    handle IllegalMove => true) (* Exception *)

val test80 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test81 = all_same_color [(Hearts, Ace)] = true
val test82 = all_same_color [] = true
val test83 = all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true
val test84 = all_same_color [(Hearts, Ace), (Clubs, Ace)] = false
val test85 = all_same_color [(Spades, King), (Clubs, Ace)] = true
val test86 = all_same_color [(Spades, King), (Clubs, Ace), (Spades, Ace)] = true

val test90 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test91 = sum_cards [(Clubs, Num 2),(Clubs, Num 3)] = 5
val test92 = sum_cards [] = 0
val test93 = sum_cards [(Clubs, Ace),(Clubs, Num 3)] = 14
val test94 = sum_cards [(Clubs, Ace),(Clubs, King)] = 21

val test100 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test101 = score ([(Hearts, Num 2),(Clubs, Num 8)],10) = 0
val test102 = score ([(Hearts, Num 2),(Clubs, Num 8)],13) = 3
val test103 = score ([(Spades, Num 7),(Clubs, Num 8)],13) = 3

val test110 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test111 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw], 42) = 3
val test112 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)], 42); false) 
                        handle IllegalMove => true)
val test113 = officiate ([(Clubs,Ace)],
                        [Draw], 42) = 15
val test114 = officiate ([(Clubs,Ace),(Clubs,Ace)],
                        [Draw,Draw], 42) = 10
val test115 = officiate ([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)],
                        [Draw,Draw,Draw], 42) = 4
val test116 = officiate ([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)],
                        [Draw,Draw,Draw,Draw], 42) = 3
val test117 = officiate ([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)],
                        [Draw,Draw,Draw,Draw,Draw], 42) = 3