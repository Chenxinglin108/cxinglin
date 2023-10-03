(* ****** ****** *)
(*
//
Assign3:
Combinator-based programming
//
DUE: the 4th of October, 2023
//
Total: 80 points + 30 bonus points
(OCaml: 50+30 points)(Python: 30 points)
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)

(* ****** ****** *)

(*
Assign3-2:
HX-2023-09-26: 10 points
//
The function [list_subsets]
returns all the subsets of a given
set (where sets are represented as lists)
//
let rec
list_subsets
(xs: 'a list): 'a list list =
(
match xs with
  [] -> [[]]
| x1 :: xs ->
  let res = list_subsets(xs) in
    res @ list_map(res, fun(xs) -> x1 :: xs)
)
//
Please give a NON-RECURSIVE implementation of
list_subsets based on list-combinators. Note that
the order of the elements in a list representation
of a set is insignificant. For instance, [1,2] and
[2,1] represents the same set {1,2}.
//
*)

(* ****** ****** *)

(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let rec
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)


#use ".assign3.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;
let list_foldleft(xs) =
  foreach_to_foldleft(list_foreach)(xs)

let list_foldright(xs) =
  rforeach_to_foldright(list_rforeach)(xs)
let
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
list_foldright(xs)([])(fun x0 r0 -> fopr(x0) :: r0)




     
    let list_subsets xs =

      list_foldleft xs [[]]  (fun acc x ->
        list_append (acc) (list_map acc (fun subset -> x :: subset)))