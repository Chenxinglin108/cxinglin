(* ****** ****** *)
(*
Assign0: Warmup!
*)
(* ****** ****** *)
#use "./../assign1.ml";;
 #use "./../../../classlib/OCaml/MyOCaml.ml";;

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0

(* ****** ****** *)



(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml returns '0' (due
to arithmetic overflow.



*)


let find = 
  let rec helper h  =
    match fact h with 
    | 0 -> h
    |_ -> helper (h+1)

in helper 1


(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether
a given natural number is a prime:
fun isPrime(n0: int): bool
*)

(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

(* ****** ****** *)

(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
for natural numbers x.
(You can assume that the given string is a sequence of digits)
(And the empty sequence represents the integer 0)
*)

(* ****** ****** *)

(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified.
*)
