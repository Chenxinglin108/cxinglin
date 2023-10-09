(* ****** ****** *)
(*
//
Assign4:
Lazy-evaluation and streams
//
DUE: the 17th of October, 2023
//
Total: 110 points
50~points(OCaml)+
60~points(Python)(including 30 bonus pts)
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)



type 'a strcon =
  StrNil
| StrCons of
  'a * (unit -> 'a strcon)

(* ****** ****** *)

type 'a stream =
unit -> 'a strcon (* thunk *)

let ln2_series_stream : float stream = 
  let rec partial_sum_generator n sum =
    let next_term = if n mod 2 = 0 then 1.0 /. float (n + 1) else -.1.0 /. float (n + 1) in
    let next_sum = sum +. next_term in
    StrCons (next_sum -.1., fun () -> partial_sum_generator (n + 1) next_sum)
  in
  fun () -> partial_sum_generator 0 1.0 



  let rec print_ln2_series stream n =
    match (n, stream ()) with
    | (0, _) -> ()
    | (_, StrNil) -> ()
    | (_, StrCons (value, rest)) ->
      Printf.printf "%.6f, " value;
      print_ln2_series rest (n - 1)
  
  let () =
    let n = 20000000 in  (* Print the first 10 partial sums *)
    print_ln2_series ln2_series_stream n;
    print_endline ""

(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: float stream = fun() -> ...
//
*)

(* ****** ****** *)

(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fun () -> ...
//
*)

type 'a strcon =
  StrNil
| StrCons of
  'a * (unit -> 'a strcon)

(* ****** ****** *)

type 'a stream =
unit -> 'a strcon (* thunk *)


let rec theNatPairs : (int * int) stream =
  fun () ->
    let rec pairs i j () =
      match (i, j) with
      | (0, _) ->
        StrCons ((i, j), fun () ->
          pairs (i + 1) (j - 1) ())
      | (a, 0) ->
        StrCons ((i, j), fun () ->
          pairs 0 (a + 1) ())
      | (_, _) ->
        StrCons ((i, j), fun () ->
          pairs (i + 1) (j - 1) ())
    in
    StrCons ((0, 0), fun () -> pairs 0 1 ())

(* Function to take the first n elements from a stream *)
let rec take n strm =
  let rec take_n n' acc s =
    match n', s with
    | 0, _ -> List.rev acc
    | _, StrNil -> List.rev acc
    | _, StrCons (x, rest) -> take_n (n' - 1) (x :: acc) (rest ())
  in
  take_n n [] strm

(* Print the first n pairs generated by enumerate_pairs *)
let print_pairs n =
  let pairs_stream = enumerate_pairs () in
  let first_n_pairs = take n pairs_stream in
  Printf.printf "[";
  List.iter (fun (i, j) -> Printf.printf "(%d, %d); " i j) first_n_pairs;
  Printf.printf "]\n"

(* Example usage: Print the first 10 pairs *)
let () =
  print_pairs 2000000






(* ****** ****** *)

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

(* ****** ****** *)





type 'a gtree =
  | GTnil
  | GTcons of 'a * ('a gtree list)

type 'a strcon =
  | StrNil
  | StrCons of 'a * (unit -> 'a strcon)

(* Depth-First Search (DFS) Enumeration *)
let rec gtree_streamize_dfs (xs: 'a gtree): 'a strcon =
  match xs with
  | GTnil -> StrNil
  | GTcons (value, children) ->
    let rec dfs_children children () =
      match children with
      | [] -> StrNil
      | hd :: tl ->
        let child_stream = gtree_streamize_dfs hd in
        StrCons (value, fun () -> dfs_children tl ())
    in
    StrCons (value, fun () -> dfs_children children ())

(* Breadth-First Search (BFS) Enumeration *)
let gtree_streamize_bfs (xs: 'a gtree): 'a strcon =
  let rec bfs_queue queue () =
    match queue with
    | [] -> StrNil
    | GTnil :: rest -> bfs_queue rest ()
    | GTcons (value, children) :: rest ->
      let child_values = List.map (fun child -> match child with GTnil -> [] | GTcons (v, _) -> [v]) children in
      let flattened_children = List.flatten child_values in
      let new_queue = rest @ children in
      StrCons (value, fun () -> bfs_queue new_queue ())
  in
  StrCons (xs, fun () -> bfs_queue [xs] ())

(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)

(* ****** ****** *)

(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)

(* ****** ****** *)

(* end of [CS320-2023-Fall-assigns-assign4.ml] *)


let ( ^^ ) e ll = List.map (fun x -> e::x) ll


let rec permut l r =  
    match r with 
    | [] -> [[]]
    | [x] -> x ^^ (permut [] (List.rev l))
    | x::t -> let s = permut (x::l) t in 
              (x ^^ (permut [] (l@t))) @ s

      


              let rec insert x lst =
                match lst with
                | [] -> [[x]]
                | h::t -> 
                  (x::lst) :: (List.map (fun el -> h::el) (insert x t));;


                  let rec perm lst =
                    match lst with
                    | [] -> [lst]
                    | h::t -> 
                      List.flatten (List.map (insert h) (perm t));;