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




(* ****** ****** *)
let
mylist_subscript_exn
( (*void*) ): 'a = raise MySubscript
(* ****** ****** *)



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



type
('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

type
('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

(* ****** ****** *)

(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* ****** ****** *)





let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)




  
  let
  foldleft_to_iforeach
  (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach= fun xs iwork ->
   
      let rec iforeach_helper i xs =
        match xs with
        | [] -> ()
        | x :: rest ->
          iwork i x ;
          iforeach_helper (i + 1) rest
      in
      iforeach_helper 0 xs

     




(* ****** ****** *)
 