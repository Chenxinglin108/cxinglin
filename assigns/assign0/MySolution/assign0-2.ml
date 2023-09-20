(*
Assign0-2: 10 points
Please implement a function that tests whether
a given natural number is a prime:
fun isPrime(n0: int): bool
*)
#use "./../assign1.ml";;
 #use "./../../../classlib/OCaml/MyOCaml.ml";;


let isPrime n=
  if n < 2 then false else 
    if n =2 then true else
    
    let rec helper n i =
      if n = i then true else
      match n mod i with
    | 0 -> false
    |_ -> helper  n (i+1)

    in helper n 2