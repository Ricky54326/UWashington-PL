


(* is_older: date * date -> bool * *)
fun is_older (a : int*int*int,  b: int*int*int)=
  true
      (* TODO: IMPLEMENT *)

(* number_in_month: returns #dates in given month *)
fun number_in_month (dates : (int*int*int) list, month: int) =
  if null dates
  then 0
  else
      if #2 (hd dates) = month
      then 1 + number_in_month (tl dates, month)
      else 0 + number_in_month (tl dates, month)
			      
      
fun number_in_months (dates: (int*int*int) list, months: int list) =
  if null months orelse null dates
  then 0
  else
      number_in_month (dates, hd months) + 
      number_in_months(dates, tl months)

		    
