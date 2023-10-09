

#use ".assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let remove elem lst = list_foldleft (list_reverse lst) [] (fun acc x-> if x <> elem then x :: acc else acc)



let rec permutations = function
  | x::[] -> [[x]]
  | l -> 
    list_foldleft l [] (fun acc x -> list_append acc (list_map (permutations (remove x l)) (fun p -> x::p)) ) 



    let list_permute(xs: 'a list): 'a list stream= 

 

    let rec helper x  = 

    match x with 

    | [] -> StrNil
    | h :: t -> StrCons (h, fun()-> helper t )

  in fun() -> helper (permutations xs)
