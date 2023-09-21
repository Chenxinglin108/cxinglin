#use "./../assign1.ml";;
 #use "./../../../classlib/OCaml/MyOCaml.ml";;


let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0

(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0






  let char_to_int c =
    match c with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> raise (Invalid_argument "Input must be a digit character '0' to '9'")


 let h c =
  let rec h1 acc n=
    if n =0 then acc else
      acc * 10  * (h1) (acc) (n-1)

    in h1 1 c 
    
let str2int(cs: string): int =

if string_length cs =1 then char_to_int (string_get(cs,0)) else
 
let rec helper acc c =

  if c=string_length cs then acc else

  char_to_int (string_get (cs,c)) *  (h ((string_length cs) -1 - c))  +  helper  acc (c+1)

  in helper 0 0






  let combine_strings str1 str2 =
    let length1 = String.length str1 in
    let length2 = String.length str2 in
    let combined_length = length1 + length2 in
    let combined_string = String.init combined_length (fun i ->
      if i < length1 then
        String.get str1 i
      else
        String.get str2 (i - length1)
    ) in
    combined_string