fun is_older(a: int*int*int, b: int*int*int) =
    if #1 a * 365 + #2 a * 31 + #3 a < #1 b * 365 + #2 b * 31 + #3 b 
    then true
    else false
(* 3 pts
Not a particular functional programming approach, but that's OK. 
The problems lies in the "if x then true else false" style, which is a bad style 
in any programming language. *)

fun number_in_month(l: (int*int*int) list, m:int) =
    if null l
    then 0
    else let
      val n = number_in_month(tl l, m)
    in
      if #2 (hd l) = m
      then 1 + n
      else n
    end    
(* 5 pts
Recursive calls are clear. Good. *)

fun number_in_months(l: (int*int*int) list, m: int list) =
    if null m
    then 0
    else number_in_month(l, hd m) + number_in_months(l, tl m) 
(* 5 pts
Good. *)

fun dates_in_month(l: (int*int*int) list, m:int) =
    if null l
    then []
    else    let
                val ll = dates_in_month(tl l, m)
            in
                if #2 (hd l) = m
                then (hd l) :: ll 
                else ll
            end
(* 5 pts
Good. *)
            
fun dates_in_months(l: (int*int*int) list, m: int list) =
    if null m 
    then []
    else dates_in_month(l, hd m) @ dates_in_months(l, tl m)
(* 5 pts
Good. *)

fun get_nth(l: string list, n: int) =
    if n = 1
    then hd l
    else get_nth(tl l, n - 1)    
(* 5 pts
Good. *)

fun date_to_string(d: int*int*int) = 
    let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
      get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end
(* 5 pts
Good. *)

fun number_before_reaching_sum(sum: int, l: int list) = 
    let
      fun helper(s: int, n: int, ll: int list) =
      if null ll 
      then 0
      else if s + hd ll >= sum
           then n 
           else helper(s + hd ll, n + 1, tl ll)
    in
      helper(0, 0, l)
    end
(* 4 pts
Could be more concise. *)

fun what_month(day: int) = 
    let
      val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      number_before_reaching_sum(day, months) + 1
    end    
(* 5 pts
Good. *)

fun month_range2(day1: int, day2: int) =
    if day1 > day2 
    then []
    else let
            fun range(a: int, b: int) =
                if a > b 
                then []
                else [a] @ range(a + 1, b)
         in
            range(what_month(day1), what_month(day2))
         end

fun month_range(day1: int, day2: int) =         
    if day1 > day2 
    then []
    else [what_month(day1)] @ month_range(day1 + 1, day2)
(* 5 pts.
Generally good, but could have used "::" operator, which is more concise. 
Doesn't have to make it a list. *)

fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE 
    else if null (tl dates)
         then SOME(hd dates)
         else let
                val old = valOf (oldest(tl dates))
              in
                if is_older(hd dates, old)
                then SOME (hd dates)
                else SOME (old)
              end
(* 5 pts
Good. *)