#use ".assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

(* ****** ****** *)
exception MySubscript
(* ****** ****** *)

(*
HX-2023-09-21:
Please read the following code to understand
the meaning of mylist.
*)

(* ****** ****** *)
 
let rec
mylist_foreach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (work(x1); mylist_foreach(xs)(work))
| MySnoc(xs, x1) ->
  (mylist_foreach(xs)(work); work(x1))
| MyReverse(xs) -> mylist_rforeach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

and
mylist_rforeach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (mylist_rforeach(xs)(work); work(x1))
| MySnoc(xs, x1) ->
  (work(x1); mylist_rforeach(xs)(work))
| MyReverse(xs) -> mylist_foreach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))

(* ****** ****** *)


let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0  
  | MyCons(_, xs') -> 1 + mylist_length xs' 
  | MySnoc(xs', _) -> 1 + mylist_length xs'  
  | MyReverse(xs') -> mylist_length xs'  
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2 

(* ****** ****** *)
let
mylist_subscript_exn
( (*void*) ): 'a = raise MySubscript
(* ****** ****** *)


let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  match (xs, i0) with
    (* Empty list, raise MySubscript *)
  | (_, n) when n < 0 -> mylist_subscript_exn ()  (* Negative index, raise MySubscript *)
  | (MyCons(x1, _), 0) -> x1  (* Found the element at the specified position *)
  | (MySnoc(_, x1), 0) -> x1
  | (MyCons(_, xs'), _) -> mylist_get_at xs' (i0 - 1)  (* Recursively search for the element *)
  | (MySnoc(xs', _), _) -> mylist_get_at xs' (i0 - 1)
  | (MyReverse(xs'), _) -> mylist_get_at (reverse_mylist(xs')) i0
  | (MyAppend2(xs1, xs2), _) ->
    let len_xs1 = mylist_length xs1 in
    if i0 < len_xs1 then mylist_get_at xs1 i0
    else mylist_get_at xs2 (i0 - len_xs1)

  | (MyNil, _) -> mylist_subscript_exn ()
(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)

let rec reverse_mylist (xs: 'a mylist): 'a mylist =
  let rec reverse_helper acc = function
    | MyNil -> acc
    | MyCons (x, rest) -> reverse_helper (MySnoc (acc, x)) rest
    | MySnoc (rest, x) -> reverse_helper (MyCons (x, acc)) rest
    | MyReverse ys -> reverse_helper acc ys
    | MyAppend2 (ys1, ys2) -> reverse_helper (MyAppend2 (reverse_helper MyNil ys2, reverse_helper MyNil ys1)) MyNil
  in
  reverse_helper MyNil xs