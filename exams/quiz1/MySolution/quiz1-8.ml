(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5: int*int*int*int*int -> int*int*int*int*int =

fun (a, b, c, d, e) ->
  let swap (x, y) = if x > y then (y, x) else (x, y) in

  let (a, b) = swap (a, b) in
  let (a, c) = swap (a, c) in
  let (a, d) = swap (a, d) in
  let (a, e) = swap (a, e) in
  let (b, c) = swap (b, c) in
  let (b, d) = swap (b, d) in
  let (b, e) = swap (b, e) in
  let (c, d) = swap (c, d) in
  let (c, e) = swap (c, e) in
  let (d, e) = swap (d, e) in

  (a, b, c, d, e)
  (* YOUR CODE *)


(* ************************************************ *)
