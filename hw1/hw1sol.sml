(* 1.   Takes two dates and evaluates to true or false.
        Returns true if first date comes before the second date.
        If two dates are equal, return false *)
fun is_older(x: int*int*int, y: int*int*int) =
    if #1 x <> #1 y
    then #1 x < #1 y
    else
        if #2 x <> #2 y
        then #2 x < #2 y
        else #3 x < #3 y


(* 2.   Takes a list of dates and a month, and returns how many dates in the 
        list are in the given month *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null(dates)
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)


(* 3.   Takes a list of dates and a list of months, and returns the number of
        dates in the list of dates that are in any of the months in the list of
        months. Assume no duplicates in months *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null(months)
    then 0
    else number_in_month(dates, hd months) +
        number_in_months(dates, tl months)


(* 4.   Takes a list of dates and a month, and returns a list holding the dates
        from the argument list of dates that are in the month. The returned list 
        should contain dates in the order they were originally given *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null(dates)
    then []
    else if #2(hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)


(* 5.   Takes a list of dates and a list of months, and returns a list holding 
        the dates from the argument list of dates taht are in any of the months 
        in the list of monbths. Assume no duplicates in months *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null(months)
    then []
    else dates_in_month(dates, hd months) @
        dates_in_months(dates, tl months)


(* 6.   Takes a list of strings and an int n, and returns the nth element of the
        list where the head of the list is 1st. *)
fun get_nth(strings: string list, n: int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)


(* 7.   Takes a date and returns a string of the form January 20, 2013. *)
fun date_to_string(date: int*int*int) =
    let
        val months = ["January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ 
        Int.toString(#1 date)
    end


(* 8.   Takes an int sum and a list, return an int such that the first n elements
        of the list add to less the sum, but the first n+1 elements of the list 
        add to sum or more *)
fun number_before_reaching_sum(sum: int, l: int list) =
    if sum <= hd l
    then 0
    else 1 + number_before_reaching_sum(sum - hd l, tl l)


(* 9.   Takes a day of a year (an int), and returns what month that day is in. *)
fun what_month(day: int) =
    let
        val l = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, l) + 1
    end


(* 10.  Takes two days of the year day1 and day2, and returns an int list
        [m1, m2, ..., mn] where m1 is the month of day1, m2 is the month of
        day1+1... *)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)


(*  11. Taakese a list of dates and evaluates to an date option. It evaluates to
        NONE if the list has no dates and SOME d if the date d is the oldest
        date in the list. *)
fun oldest(dates: (int*int*int) list) =
    if null(dates)
    then NONE
    else
        let
            fun oldest_nonempty(dates: (int*int*int) list) =
                if null(tl dates)
                then hd dates
                else
                    let
                        val candidate = oldest_nonempty(tl dates)
                    in
                        if is_older(hd dates, candidate)
                        then hd dates
                        else candidate
                end
        in
            SOME (oldest_nonempty(dates))
        end


(* 12.  Functions behave like problem 3 and 5 except having a month in the
        second argument multiple times have no more effect than having it
        once *)
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    let
        fun remove_duplicates(l: int list) =
        if null(l)
        then []
        else if null(tl l)
        then l
        else
            let
                fun compare(x: int, y: int) = x > y
                fun sort(l: int list) = ListMergeSort.sort(compare) l
                val l_sorted = sort(l)
            in
                if (hd l_sorted) = (hd (tl l_sorted))
                then remove_duplicates(tl l_sorted)
                else (hd l_sorted)::remove_duplicates(tl l_sorted)
            end
        val months_unique = remove_duplicates(months)
    in
        number_in_months(dates, months_unique)
    end

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    let
        fun remove_duplicates(l: int list) =
        if null(l)
        then []
        else if null(tl l)
        then l
        else
            let
                fun compare(x: int, y: int) = x > y
                fun sort(l: int list) = ListMergeSort.sort(compare) l
                val l_sorted = sort(l)
            in
                if (hd l_sorted) = (hd (tl l_sorted))
                then remove_duplicates(tl l_sorted)
                else (hd l_sorted)::remove_duplicates(tl l_sorted)
            end
        val months_unique = remove_duplicates(months)
    in
        dates_in_months(dates, months_unique)
    end

(* 13.  Function behave like problem 5, except having a month in the
        second argument multiple times have no more effect than having it
        once *)
fun reasonable_date(date: int*int*int) =
    let
        fun is_leap_year(year: int) =
            (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))
        fun is_valid_day(date: int*int*int) =
            if #2 date = 1 orelse
                #2 date = 3 orelse
                #2 date = 5 orelse
                #2 date = 7 orelse
                #2 date = 8 orelse
                #2 date = 10 orelse
                #2 date = 12
            then #3 date <= 31
            else if #2 date = 4 orelse
                #2 date = 6 orelse
                #2 date = 9 orelse
                #2 date = 11
            then #2 date <= 30
            else if is_leap_year(#1 date)
            then #3 date <= 29
            else #3 date <= 28
    in
        #1 date > 0 andalso 
        #2 date > 0 andalso #2 date <= 12 andalso
        #3 date > 0 andalso
        is_valid_day(date)
    end