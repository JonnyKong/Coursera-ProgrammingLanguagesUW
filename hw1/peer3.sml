fun get_year(date: int*int*int) =
    #1 date

fun get_month(date: int*int*int) =
    #2 date

fun get_day(date: int*int*int) =
    #3 date

(* (int*int*int) * (int*int*int) -> bool *)
fun is_older(date1: int*int*int, date2: int*int*int) =
    if get_year(date1) = get_year(date2) then
       if get_month(date1) = get_month(date2) then
	  if get_day(date1) = get_day(date2) then
	      false
	  else get_day(date1) < get_day(date2)
       else get_month(date1) < get_month(date2)
    else get_year(date1) < get_year(date2)
(* 5 pts
Logic is clear. But bad indentation makes it hard to read. *)

(* (int*int*int) list * int -> int *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates then 0
    else
	let
	    val x = number_in_month(tl dates, month)
	in
	    if get_month(hd dates) = month
	    then 1 + x
	    else x
	end
(* 5 pts
Good. However a bit too imperative-style. *)

(* (int*int*int) list * int list -> int *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months)
(* 5 pts
Good. *)

(* (int*int*int) list * int -> int list *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates then []
    else
	let
	    val lst = dates_in_month(tl dates, month)
	in
	    if get_month(hd dates) = month
	    then (hd dates)::lst
	    else lst
	end
(* 5 pts
Good. *)

(* (int*int*int) list * int list -> int list *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months then []
    else
	let
	    val head = dates_in_month(dates, hd months)			(* use dates_in_month function on the head *)
	    val tail_list = dates_in_months(dates, tl months)		(* recurse on tail *)
	in
	    if null head
	    then tail_list
	    else head @ tail_list
	end
(* 5 pts
Good. *)

(* string list * int -> string *)
fun get_nth(strings: string list, n: int) =
    if n = 1 then hd strings
    else
	get_nth(tl strings, n-1)
(* 5 pts
Good. *)

(* int*int*int -> string *)
fun date_to_string(date: int*int*int) =
    let
	val months_eng = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months_eng,get_month(date)) ^ " " ^ Int.toString(get_day(date)) ^ ", " ^ Int.toString(get_year(date))
    end
(* 5 pts
Good. *)

(* int * int list -> int *)
fun number_before_reaching_sum(sum: int, num_list: int list) =
    let
	fun helper(temp_sum: int, temp_list : int list) =				(* Recursively go through list adding to the temp sum and stop if it exceeds the sum *)
	    if temp_sum >= sum then 0
	    else
		1 + helper(temp_sum + hd temp_list, tl temp_list)
    in
	helper(hd num_list, tl num_list)
    end
(* 5 pts
Good. *)

(* int -> int *)
fun what_month(day_of_year: int) =
    let
	val numdays_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day_of_year,numdays_month) + 1		(* off by 1 *)
    end
(* 5 pts
Good. *)

(* int * int -> int list *)
fun month_range(day1: int, day2: int) =
    if day1 > day2 then []
    else
	what_month(day1)::month_range(day1+1, day2)
(* 5 pts
Good. *)

(* (int*int*int) list -> int option *)
fun oldest(dates: (int*int*int) list) =
    if null dates then NONE
    else
	let
	    val x = oldest(tl dates)
	in
	    if not (isSome x) orelse is_older(hd dates, valOf x)
	    then SOME (hd dates)
	    else x
	end
(* 5 pts
Good. *)

(* CHALLENGES *)

(* int * int list -> bool *)
fun in_list(a: int, lst: int list) =						(* recurse through list *)
    if null lst then false
    else a = hd lst orelse in_list (a, tl lst)

(* int list -> int list *)
fun strip_duplicates(num_list: int list) =
    let
	fun helper(lst1: int list, lst2: int list) =			(* recurse through lst2, add to lst1 unless it's already inside *)
	    if null lst2 then lst1
	    else
		if in_list(hd lst2, lst1)
		then helper(lst1,tl lst2)
		else helper((hd lst2)::lst1,tl lst2)
    in
	helper([],num_list)
    end

(* (int*int*int) list * int list -> int *)	
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    let
	val stripped_months = strip_duplicates(months)
    in
	number_in_months(dates,stripped_months)
    end
(* 5 pts
You have missed another function, but that's okay. *)

(* int*int*int -> bool *)
fun reasonable_date(date: int*int*int) =
    let
	val y = get_year(date)
	val m = get_month(date)
	val d = get_day(date)
    in
	if y <= 0 orelse m <= 0 orelse m >= 13 orelse d <= 0
	then false
	else
	    if m = 1 orelse m = 3 orelse m = 5 orelse m = 7 orelse m = 8 orelse m = 10 orelse m = 12
	    then d <= 31
	    else if m = 4 orelse m = 6 orelse m = 9 orelse m = 11
	    then d <= 30
	    else (*m = 2*)
		if y mod 4 = 0 then d <= 29
		else d <= 28
    end
(* 4 pts
Incorrect in which years are leap year. *)