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

fun similar_names(subs: string list list, {first=f, middle=m, last=l}: name_record) =
  let
      fun aux(subs:string list, new_names) =
	      case subs of
	          [] => new_names
	        | x::xs' => let val name = {first=x,middle=m,last=l}
                      in aux(xs', name::new_names)
                      end

      val substitutions = get_substitutions1(subs, f)
  in
      rev(aux(substitutions, [{first=f,middle=m,last=l}]))
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black


exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card) =
  case card of
      (Clubs, _) => Black
    | (Spades, _)  => Black
    | _ => Red

fun card_value(card) =
  case card of
      Num n => n
    | Ace => 1
    | _  => 10

fun remove_card(cs, c, e) =
  case cs of
      [] => raise e
    | x::xs' => if x = c then xs' else x::remove_card(xs', c, e)

fun all_same_color(cs) =
  let fun aux(cs, color) =
        case cs of
            [] => true
         | x::xs' => if card_color(x) = color then aux(xs', color) else false
  in case cs of
         [] => true
      |  x::xs'  => aux(xs', card_color(x))
  end

fun all_same_color2(cs) =
  case cs of
      [] => true
   |  x::xs'  =>

      let val color = card_color(x)
          fun aux(cs) =
            case cs of
                [] => true
              | x::xs' => if card_color(x) = color then aux(xs') else false
      in aux(xs')
      end

fun sum_cards(cs) =
  let fun aux(cs, sum) =
        case cs of
            [] => sum
          | x::xs' => aux(xs', sum+card_value(x))

  in aux(cs, 0)
  end

fun score(cs, goal) =
  let sum = sum_cards(cs)
  in
