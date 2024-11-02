(* Dan Grossman, Coursera PL, HW0 Provided Code *)

(* The line below is wrong -- replacing the addition, +, with
multiplication, *, will fix it *)
fun f(x,y) = x * y

(* Do not change these: They should be correct after fixing the code above *)

fun double x = f(x,2)

fun triple x = f(3,x)

val test1 = double 17 = 34

val test2 = double 0 = 0

val test3 = triple ~4 = ~12

val test4 = triple 0 = 0

val test5 = f(12,27) = 324
