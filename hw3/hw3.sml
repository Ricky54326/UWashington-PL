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
fun longest_string1(xs:string list) =
  let
      fun larger(x1, x2) =
        if String.size(x1) > String.size(x2)
        then x1
        else x2
  in List.foldl larger "" xs
  end


(* 3 *)
fun longest_string2(xs: string list) =
  let
      fun larger(x1, x2) =
        if String.size(x1) >= String.size(x2)
        then x1
        else x2
  in List.foldl larger "" xs
  end

(* 4 *)
fun longest_string_helper (f:(int*int->bool)) (xs:string list): string =
  let
      fun larger(x1, x2) =
        let
            val l1 = String.size(x1);
            val l2 = String.size(x2);
        in
            if f(l1, l2) then x1 else x2
        end
  in
      List.foldl larger "" xs
  end

fun gt (x, y) =  x > y
fun gte (x, y) = x >= y

val longest_string3 = longest_string_helper gt
val longest_string4 = longest_string_helper gte

(* 5 TODO: Need to use composition*)
fun longest_capitalized(xs: string list) =
  let
      val uppers = only_capitals xs;
  in
      longest_string_helper gt uppers
  end

(* 6 *)
val rev_string = String.implode o rev o String.explode


(* 7 *)
fun first_answer ( f:('a -> 'b option)) (xs:'a list) =
  let
      val xs' = List.map f xs;
  in
      valOf (hd (List.filter isSome xs'))
  end
