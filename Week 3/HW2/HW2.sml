(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* fun all_except_option  *)
fun all_except_option(str : string, lst : string list) =
    case lst of
        [] => NONE
      | x::xs =>
        case same_string(x, str) of
            true => SOME xs
          | false =>
            case all_except_option(str, xs) of
                NONE => NONE
              | SOME y => SOME (x::y)


(* get_substitutions1  *)
fun get_substitutions1 ([], s) = []
| get_substitutions1 (x :: xs, s) =
    case all_except_option(s, x) of
      NONE => get_substitutions1(xs, s)
    | SOME y => y @ get_substitutions1(xs, s)

(* get_substitutions2  *)
fun get_substitutions2 (substitutions : string list list, s : string) =
let
    fun aux ([], acc) = acc
      | aux (x :: xs, acc) =
        case all_except_option (s, x) of
            NONE => aux (xs, acc)
          | SOME y => aux (xs, y @ acc)
in
    aux (substitutions, [])
end


(* similar_names  *)
fun similar_names(list_substitutions, {first=x, middle=y, last=z}) =
    let
        fun helper_fun([]) = []
          | helper_fun(head :: tail) =
            {first = head, middle = y, last = z} :: helper_fun(tail)
    in
        helper_fun(x :: get_substitutions2(list_substitutions, x))
    end

(* Section 2 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* fun card_color  *)
fun card_color(card: card): color =
    case card of
        (Spades, _) => Black
      | (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

(* fun card_value  *)
fun card_value(card: card): int =
case #2 card of
    Ace => 11
  | Num n => n
  | _ => 10

(* fun remove_card  *)
fun remove_card(cs: card list, c: card, e: exn): card list =
case cs of
    [] => raise e  (* If the list is empty, raise the exception *)
  | hd :: tl =>
    if hd = c then tl  (* Remove the first occurrence of card c *)
    else hd :: remove_card(tl, c, e)


(* all_same_color  *)
fun all_same_color cs =
case cs of
  []                   => true
| head::[]             => true
| head::(second::rest) => if (card_color(head) = card_color(second))
                          then true andalso all_same_color(second::rest)
                          else false
                      

(* fun sum_cards  *)
fun sum_cards(cards: card list): int =
let
    fun tsum ([], acc) = acc
      | tsum (x::xs, acc) = tsum (xs, card_value(x) + acc)
in
    tsum (cards, 0)
end


(* fun score  *)
fun score (cards, goal) =
  let
      val initial_sum = sum_cards(cards)
      val ascore = if initial_sum > goal
			 then 3 * (initial_sum - goal)
			 else (goal - initial_sum)
				  
  in
      if all_same_color(cards)
      then ascore div 2
      else ascore	       
  end

(* fun officiate  *)
fun officiate (hand, actions, target) =
  let
      fun playgame (currentHand, target, remainingCards, remainingActions) =
          if sum_cards(currentHand) > target
          then score(currentHand, target)
          else case remainingActions of
                   [] => score(currentHand, target)
                 | action::restActions => case action of
                                            Discard(card) => playgame (remove_card(currentHand, card, IllegalMove), target, remainingCards, restActions)
                                          | Draw => case remainingCards of
                                                      [] => score(currentHand, target)
                                                    | card::[] => playgame(card::currentHand, target, [], restActions)
                                                    | card::restCards => playgame(card::currentHand, target, restCards, restActions)
  in
      playgame([], target, hand, actions)
  end
