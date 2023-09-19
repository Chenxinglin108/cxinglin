
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)
 

(* ****** ****** *)
(*
  MyOCaml.ml is a library
  built for CS320, Fall, 2023
*)
(* ****** ****** *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let isPrime(n) =
  let test(i:int): bool = 
  let rec helper i (n)= 
  if n= 2 then true else 
    if i=n then true else
    match n mod i with
    | 0 -> false
    | _ -> helper (i+1) n

    in helper 2 n
  in
  if n < 2 then false else int1_forall(n)(test)

(* ************************************************ *)
