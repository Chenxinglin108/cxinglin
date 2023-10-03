

#use ".assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;


let list_foldleft(xs) =
  foreach_to_foldleft(list_foreach)(xs)

let list_foldright(xs) =
  rforeach_to_foldright(list_rforeach)(xs)
let
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
list_foldright(xs)([])(fun x0 r0 -> fopr(x0) :: r0)


let list_length xs= list_foldleft xs 0 (fun acc _-> acc+1)

let list_subsets xs =

  list_foldleft xs [[]]  (fun acc x ->
    list_append (acc) (list_map acc (fun subset -> x :: subset)))


let helper xs = list_foldleft xs [] (fun acc x -> (list_reverse x) :: acc )

let list_nchoose (xs: 'a list)(n0: int): 'a list list = let x =
  let l= list_subsets xs in list_foldleft l [] (fun acc x -> if list_length x = n0 then x::acc else acc ) in helper x


