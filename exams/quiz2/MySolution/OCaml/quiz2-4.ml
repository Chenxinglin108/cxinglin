(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

exception Empty


exception Empty
let list_length xs= list_foldleft xs 0 (fun acc x -> acc +1 )
let list_last(xs: 'a list): 'a =  

  match list_foldleft xs None (fun acc elem -> Some elem) with
  | Some last_elem -> last_elem
  | None -> raise Empty
