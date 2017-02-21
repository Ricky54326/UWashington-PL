(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


fun first_ch x = String.sub(x, 0)

(* 1 *)
fun only_capitals(xs: string list) =
   List.filter(Char.isUpper o first_ch) xs

(* 2 *)
fun longest_string1(xs) =
  let
      fun larger(x1, x2) =
        if String.size(x1) > String.size(x2)
        then x1
        else x2
  in List.foldl larger "" xs
  end


(* 3 *)
fun longest_string2(xs) =
  let
      fun larger(x1, x2) =
        if String.size(x1) >= String.size(x2)
        then x1
        else x2
  in List.foldl larger "" xs
  end
