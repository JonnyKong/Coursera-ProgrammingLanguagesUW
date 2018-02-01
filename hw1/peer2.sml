(* Homework1 Solution*)

(* Task 1 *)
fun is_older(date1 : (int * int * int), date2 : (int * int * int)) =
    ((#1 date1 < #1 date2))
    orelse
    ((#1 date1 = #1 date2) andalso (#2 date1 < #2 date2))
    orelse
    ((#1 date1 = #1 date2) andalso (#2 date1 = #2 date2) andalso (#3 date1 < #3 date2));
(* 5 pts
Good. *)

(* Task 2 *)
fun number_in_month(dates : (int * int * int) list, month : int) =
    if	null dates
    then 0
    else
	(if (#2 (hd dates)) = month then 1 else 0) + number_in_month(tl dates, month);
(* 5 pts
Good. *)

(* Task 3 *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
    if	null months
    then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months);
(* 5 pts
Good. *)

(* Task 4 *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
    if	null dates
    then []
    else
	if
	    (#2 (hd dates)) = month
	then
	    (hd dates)::(dates_in_month(tl dates, month))
	else
	    dates_in_month(tl dates, month);
(* 5 pts
Good. *)

(* Task 5 *)
fun dates_in_months(dates : (int * int * int) list,months : int list) =
    if	null months
    then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);
(* 5 pts.
Good. *)
       
(* Task 6 *)
fun get_nth(strings : string list, n : int) =
    if	n = 1
    then hd strings
    else get_nth(tl strings, n - 1);
(* 5 pts
Good. *)

(* Task 7 *)
fun date_to_string(date : (int * int * int)) =
    let
	val month_names = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end;
(* 5 pts
Good. *)

(* Task 8 *)
fun number_before_reaching_sum(sum : int, numbers : int list) =
    if	sum <= hd numbers
    then 0
    else
	1 + number_before_reaching_sum(sum - (hd numbers), tl numbers);
(* 5 pts
Good. *)

(* Task 9 *)
fun what_month(day : int) =
    let
	val num_days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, num_days_in_months)
    end;
(* 5 pts
Good. *)

(* Task 10 *)
fun month_range(day1 : int, day2 : int) =
    if	day2 < day1
    then []
    else
	what_month(day1)::month_range(day1 + 1, day2);
(* 5 pts
Good. *)

(* Task 11 *)
fun oldest(dates : (int * int * int) list) =
    if	null dates
    then NONE
    else
	let
	    val tl_oldest_date = oldest(tl dates)
	    fun oldest_nonempty(dates : (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else
		    let
			val tl_ans = oldest_nonempty(tl dates)
		    in
			if is_older(hd dates, tl_ans)
			then hd dates
			else tl_ans
		    end
			
	in
	    SOME (oldest_nonempty dates)
	end;
(* 5 pts
Good. *)


(* Task 12 *)

(* Helper function *) 
fun remove_duplicates(numbers : int list) =
    let
	fun contains(numbers : int list, n : int) =
	    if null numbers
	    then false
	    else (hd numbers) = n orelse contains(tl numbers, n)
    in
	if null numbers
	then []
	else if contains(tl numbers, hd numbers)
	then remove_duplicates(tl numbers)
	else (hd numbers)::remove_duplicates(tl numbers)
    end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months));
		     
fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months));
(* 5 pts
Good. *)

(* Task 13 *)
fun resonable_date(date : (int * int * int)) =
    let
	fun valid_year() =
	    #1 date > 0;

	fun valid_month() =
	    (#2 date) >= 1 andalso (#2 date) <= 12;

	fun valid_day() =
	    let		
		fun is_leap() =
		    (((#1 date) mod 400) = 0)
		    orelse
		    (((#1 date) mod 4) = 0 andalso ((#1 date) mod 100) <> 0)

		fun get_days_in_months() =
		    if is_leap()
		    then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		    else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

		fun get_nth_int(numbers : int list, n : int) =
		    if
			n = 1				
		    then			
			hd numbers
		    else			
			get_nth_int(tl numbers, n - 1);

	    in
		(#3 date) >= 1
		andalso
		(#3 date) <= get_nth_int(get_days_in_months(), #2 date)
	    end
    in
	valid_year() andalso valid_month() andalso valid_day()
    end;	
(* 5 pts
Good. However you have mistyped your function name lol. *)