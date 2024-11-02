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

(* fun only_capitals *)
fun only_capitals (strs : string list) =
List.filter (fn x => Char.isUpper (String.sub (x, 0))) (strs)

(* fun longest_string1 *)
fun longest_string1 (strs : string list) =
case strs of
    [] => ""
  | _ =>
    let
        fun maxLen (s1, s2) =
            if String.size s1 >= String.size s2 then s1 else s2
    in
        foldl maxLen "" strs
    end

(* fun longest_string2 *)
fun longest_string2 (strs : string list) =
case strs of
    [] => ""
  | _ =>
    let
        fun maxLen (s1, s2) =
            if String.size s1 >= String.size s2 then s1 else s2
    in
        foldl maxLen "" strs
    end

(* functions longest_string_helper, longest_string3, and longest_string4*)
fun longest_string_helper f sList =
		foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else acc) 
			   "" sList

val longest_string3 = longest_string_helper (fn (longf, longs) => (longf > longs))

val longest_string4 = longest_string_helper (fn (longf, longs) => (longf >= longs))

(* fun longest_capitalized *)
val longest_capitalized = longest_string1 o only_capitals

(* fun rev_string *)
val rev_string =
let
    val explodeString = String.explode
    val implodeList = String.implode
in
    implodeList o rev o explodeString
end

exception NoAnswer

(* fun first_answer *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x :: rest =>
        (case f x of
             SOME v => v
           | NONE => first_answer f rest)


(* fun all_answers *)
fun all_answers f xs =
let
    fun appendOptions (NONE, _) = NONE
      | appendOptions (_, NONE) = NONE
      | appendOptions (SOME lst1, SOME lst2) = SOME (lst1 @ lst2)
in
    List.foldl appendOptions (SOME []) (List.map f xs)
end

(* fun count_wildcards *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(* fun count_wild_and_variable_lengths *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* fun count_some_var *)
fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* fun check_pat *)
fun check_pat sol = 
    let
        fun all_strings sol =
          case sol of 
            Variable x                    => [x]
          | TupleP ps => foldl (fn (sol, i) => all_strings sol@i) [] ps
          | ConstructorP (_, sol)           => all_strings sol
          | _                             => []
      fun no_repeates strs = 
        case strs of
          []       => true
          | (s::strs') => not (List.exists (fn x => s = x) strs') andalso no_repeates strs'
    in
      no_repeates (all_strings sol)
    end

(* fun match *)
fun match (v, p) =
    case (v, p) of
      (_, Wildcard) => SOME []
    | (sv, Variable sp) => SOME [(sp,sv)]
    | (Unit, UnitP) => SOME []
    | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
    | (Tuple tv, TupleP tp) => if List.length tv = List.length tp
                               then all_answers match (ListPair.zip(tv, tp))
                               else NONE
    | (Constructor (s1,cv), ConstructorP (s2,cp)) => if s1 = s2
                                                     then match (cv,cp)
                                                     else NONE
    | (_, _) => NONE

(* fun first_match *)
fun first_match v ps = 
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE

