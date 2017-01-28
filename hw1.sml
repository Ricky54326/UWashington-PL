


(* is_older: date * date -> bool * *)
fun is_older (a : int*int*int,  b: int*int*int) =
  #1 a < #1 b orelse
  (#2 a < #2 b andalso #1 a = #1 b) orelse
  (#1 a = #1 b andalso #2 a = #2 b andalso #3 a < #3 b)


fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null dates orelse null months
  then []
  else
      let fun date (dates: (int*int*int) list, month: int) =	    
	    if null dates 
	    then []
	    else
		if #2 (hd dates) = month
		then hd dates :: date (tl dates, month)
		else date (tl dates, month)
      in
	  date(dates, hd months) @
	  dates_in_months(dates, tl months)
      end


fun dates_in_month (dates: (int*int*int) list, month: int) =
  dates_in_months(dates, [month])

		 
(* number_in_month: returns dates in given month *)
fun number_in_month (dates : (int*int*int) list, month: int) =
  length (dates_in_months(dates, [month]))
			      
      
fun number_in_months (dates: (int*int*int) list, months: int list) =
  length (dates_in_months(dates, months))
	             
	  
(* get nth string from list of strings *)
fun get_nth (strs: string list, n: int) =
  let fun count(from:int, to:int, strs: string list) =
	if from=to
	then hd strs
	else count(from+1, to, tl strs)
  in
      count(1, n, strs)
  end 
		    

(* converts date into string *)
fun date_to_string (date:(int*int*int) ) =
let
    val months = ["January", "Feburary", "March", "April", "May", "June",
	      "July", "August", "September", "October", "November", "December"]


    val month = get_nth (months, #2 date )

in
     month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
end 
		    

		    

(* Takes sum, returns n where sum of l[0,1..n-1] <= sum, but sum of l[..n] > *)
fun number_before_reaching_sum (sum: int, nums: int list) =
  if sum <= 0
  then ~1
  else 1 + number_before_reaching_sum (sum - hd nums, tl nums)


val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
				      
fun what_month (day : int) =
  number_before_reaching_sum (day, month_lengths) + 1
  
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)


				     
fun oldest (dates: (int*int*int) list):(int*int*int) option =
  let fun work (dates: (int*int*int) list, best: (int*int*int) ) =
	if null dates
	then best
	else work (tl dates, if is_older (hd dates, best) then hd dates else best )
  in
      if null dates
      then NONE
      else SOME (work (tl dates, hd dates))
  end 
