
#use ".assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

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