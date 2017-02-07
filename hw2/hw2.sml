(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun rev lst =
  let fun aux(lst,acc) =
	case lst of
	    [] => acc
	  | x::xs => aux(xs, x::acc)
  in
      aux(lst,[])
  end
	     
(* put your solutions for problem 1 here *)
fun all_except_option(str, str_list) =
  let fun aux(str_list, str_list_begin) =
	case str_list of
	    [] => NONE
	  | x::xs' => if same_string (str, x)
		      then SOME ( rev ( str_list_begin ) @ xs' ) 
		      else aux(xs', x :: str_list_begin )

  in aux(str_list, [])
  end


fun get_substitutions1(lists, str) =
  case lists of
      [] => []
    | x::xs' => case all_except_option(str, x) of
		    SOME s => s @ get_substitutions1(xs', str)
		  | NONE   => get_substitutions1(xs', str)


fun get_substitutions2(lists, str) =
  let fun aux(lists, str, newlist) =
	case lists of
	    [] => newlist
	  | x::xs' => case all_except_option(str, x) of
			  SOME s => aux(xs', str, newlist @ s)
			| NONE  => aux(xs', str, newlist)
  in aux(lists, str, [])
  end

type name_record = { first: string,
		     middle: string,
	             last:string }
	
fun similar_names(subs: string list list, fullname: name_record) =
  let
      fun aux(subs:string list, fullname:name_record, new_names) =
	case subs of 
	    [] => new_names
	  | x::xs' => aux(xs', 
      val {first=x, middle=y, last=z} = fullname
      val substitutions = get_substitutions1(subs, x)
  in
      aux(substitutions, fullname, [])
  end
      
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black


exception IllegalMove

(* put your solutions for problem 2 here *)
