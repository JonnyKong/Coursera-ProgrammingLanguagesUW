(* ----- Test cases ----- *)
val test10 = is_older ((1,2,3),(2,3,4)) = true
val test11 = is_older ((1,2,3),(1,2,4)) = true
val test12 = is_older ((1,2,3),(1,2,3)) = false
val test13 = is_older ((2,3,4),(1,2,3)) = false
val test14 = is_older ((2016,12,31),(2017,1,1)) = true

val test20 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test21 = number_in_month ([(2012,2,28),(2013,12,1)],12) = 1
val test22 = number_in_month ([],12) = 0
val test23 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2

val test30 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
                            [2,3,4]) = 3
val test31 = number_in_months ([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],
                            [2,3,4]) = 4
val test32 = number_in_months ([],[2,3,4]) = 0
val test33 = number_in_months ([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],
                            []) = 0

val test40 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test41 = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]

val test50 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
            [2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test51 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
            [2,3]) = [(2012,2,28),(2011,3,31)]

val test60 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test61 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test62 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"

val test70 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test71 = date_to_string (2017, 2, 15) = "February 15, 2017"

val test80 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test81 = number_before_reaching_sum (11, [1,2,3,4,5]) = 4

val test90 = what_month 70 = 3
val test91 = what_month 31 = 1
val test92 = what_month 32 = 2
val test93 = what_month 365 = 12
val test94 = what_month 334 = 11

val test100 = month_range (31, 34) = [1,2,2,2]
val test101 = month_range (31, 31) = [1]

val test110 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test111 = oldest([(2012,2,28),(2011,3,31),(2001,4,28)]) = SOME (2001,4,28)

(* val test120 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                                (2011,4,28)], [2,3,4]) = 3
val test121 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                                (2011,4,28)], [2,3,4,3,2,3,4,3]) = 3 *)
(* val test122 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                (2011,4,28)], [2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test123 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                (2011,4,28)], [2,3,4,3,2]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)

(* val test130 = reasonable_date((2017,12,31)) = true
val test131 = reasonable_date((2017,1,1)) = true
val test132 = reasonable_date((2017,0,1)) = false
val test133 = reasonable_date((0,1,1)) = false
val test134 = reasonable_date((2017,2,29)) = false
val test135 = reasonable_date((2017,2,28)) = true
val test136 = reasonable_date((2016,2,29)) = true
val test137 = reasonable_date((0,2,29)) = false
val test138 = reasonable_date((1600,2,29)) = false *)