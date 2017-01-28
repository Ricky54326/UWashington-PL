
type date = int*int*int
			

val months = [("January", 31),
	      ("February", 28),
	      ("March", 31),
	      ("April", 30),
	      ("May", 31),
	      ("June", 30),
	      ("July", 31),
	      ("August", 31),
	      ("September", 30),
	      ("October", 31),
	      ("November", 30),
	      ("December", 31)]


			
(* returns true if any conditions are true *) 
fun any (conds : bool list) =
  if null conds
  then false
  else hd conds orelse any(tl conds)   
			
(* is_older: date * date -> bool * *)
fun is_older (a : date,  b: date) =
  any ([#1 a < #1 b,
      (#2 a < #2 b andalso #1 a = #1 b),
      (#1 a = #1 b andalso #2 a = #2 b andalso #3 a < #3 b)])

fun is_in_month (month) =
  let fun is_in_month_date(d:date) =	
	#2 d = month
  in
      is_in_month_date
  end

fun filter (lst, f) =
  if null lst
  then []
  else
      if f (hd lst)
      then hd lst :: filter(tl lst, f)
      else filter(tl lst, f)
	   
fun dates_in_months (dates: (date) list, months: int list) =
  if null dates orelse null months
  then []
  else
      filter(dates, is_in_month(hd months)) @
      dates_in_months(dates, tl months)
     
     
fun dates_in_month (dates: (date) list, month: int) =
  dates_in_months(dates, [month])

		 
(* number_in_month: returns dates in given month *)
fun number_in_month (dates : (date) list, month: int) =
  length (dates_in_months(dates, [month]))
			      
      
fun number_in_months (dates: (date) list, months: int list) =
  length (dates_in_months(dates, months))
	             
	  
(* get nth string from list of strings *)
fun get_nth (strs, n) =
  let fun count(from, to, strs) =
	if from=to
	then hd strs
	else count(from+1, to, tl strs)
  in
      count(1, n, strs)
  end 
		    

(* converts date into string *)
fun date_to_string (date:(date) ) =
let
    
    val month = #1 (get_nth (months, #2 date))

in
     month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
end 
		    

		    

(* Takes sum, returns n where sum of l[0,1..n-1] <= sum, but sum of l[..n] > *)
fun number_before_reaching_sum (sum: int, nums: int list) =
  if sum <= 0
  then ~1
  else 1 + number_before_reaching_sum (sum - hd nums, tl nums)

				      
fun map_ (lst, f) =
  if null lst
  then []
  else f (hd lst) :: map_(tl lst, f)
				      
fun what_month (day : int) =
  number_before_reaching_sum (day, map_(months, #2)) + 1
  
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

				     
fun oldest (dates: (date) list):(date) option =
  let fun work (dates: (date) list, best: (date) ) =
	if null dates
	then best
	else work (tl dates, if is_older (hd dates, best) then hd dates else best )
  in
      if null dates
      then NONE
      else SOME (work (tl dates, hd dates))
  end 
