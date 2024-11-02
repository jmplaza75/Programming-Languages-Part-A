(* function 1: fun is_older *)
fun is_older (date1 : int * int * int, date2 : int * int * int) = 
    if (#1 date1 < #1 date2) then true
    else if (#1 date1 = #1 date2) then
        if (#2 date1 < #2 date2) then true
        else if (#2 date1 = #2 date2) then
            if (#3 date1 < #3 date2) then true
            else false
        else false
    else false;
    

(* ************************************************ *)
(* function 2: fun is_older *)
fun number_in_month(datelist : (int*int*int) list, month : int) =
    if null datelist then
        0
    else if #2 (hd datelist) = month then
        1 + number_in_month(tl datelist, month)
    else
        number_in_month(tl datelist, month)


(* ************************************************ *)
(* function 3: fun number_in_months *)
fun number_in_months (list_dates : (int * int * int) list, months : int list) =
let
    fun counter (dates, months, accumulator) =
        (* case for pattern match *)
        case dates of
            [] => accumulator (* Count of dates in the accumulator. Base case *)
          | (y, m, d)::rest => if List.exists (fn x => x = m) months then counter(rest, months, accumulator+1)
                               else counter(rest, months, accumulator)
in
    counter(list_dates, months, 0)
end;


(* ************************************************ *)
(* function 4: fun dates_in_month *)
fun dates_in_month(datelist : (int*int*int) list, month : int) =
let
    fun is_in_month(date : (int*int*int)) =
        #2 date = month
in
    List.filter is_in_month datelist
end


(* ************************************************ *)
(* function 5: fun dates_in_months *)
fun dates_in_months(datelist : (int*int*int) list, months : int list) =
let
    fun is_in_month(date : (int*int*int), month : int) =
        #2 date = month

    fun filter_by_month(date : (int*int*int)) =
        List.exists (fn m => is_in_month(date, m)) months
in
    List.filter filter_by_month datelist
end


(* ************************************************ *)
(* function 6: fun dates_in_months *)
fun get_nth(lst : string list, n : int) =
if n = 1 then
    hd lst
else
    get_nth(tl lst, n - 1)



(* ************************************************ *)
(* function 7: fun dates_in_months *)
fun date_to_string(date : (int * int * int)) =
    let
        val months = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December"]

        val (year, month, day) = date
        val month_name = get_nth(months, month)  (* Get the month name *)

        val formatted_date = month_name ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
    in
        formatted_date
    end


(* ************************************************ *)
(* function 8: fun number_before_reaching_sum *)

fun number_before_reaching_sum(sum : int, numbers : int list) =
  if hd numbers >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)



(* ************************************************ *)
(* function 9: fun what_month *)

fun what_month(day : int) =
let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
in
    1 + number_before_reaching_sum(day, days_in_month)
end


(* ************************************************ *)
(* function 10: fun month_range *)
fun month_range(day1 : int, day2 : int) =
if day1 > day2
then []
else what_month(day1) :: month_range(day1 + 1, day2)


(* ************************************************ *)
(* function 11: fun oldest *)
fun oldest(datelist : (int * int * int) list) =
    let
        fun is_older(date1 : (int * int * int), date2 : (int * int * int)) =
            #1 date1 < #1 date2 orelse
            (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
            (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

        fun find_oldest([], oldest_date : (int * int * int)) = oldest_date
          | find_oldest(hd :: tl, oldest_date) =
                if is_older(hd, oldest_date) then
                    find_oldest(tl, hd)
                else
                    find_oldest(tl, oldest_date)
    in
        case datelist of
            [] => NONE
          | hd :: tl => SOME (find_oldest(tl, hd))
    end