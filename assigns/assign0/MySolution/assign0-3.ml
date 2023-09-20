

(*Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let chr = Char.chr

let ord = Char.code
let str(c0) = String.make 1 c0

(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0




let int_to_char n =
  match n with
  | 0-> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | _ -> raise (Invalid_argument "Input must be in the range 0 to 9")






      let len n=
      let rec helper i c=
        if i =0 then c
        else  helper (i/10) (c+1)
       in helper n 0
   

       let rec help n i=
       if i=0 || i<0 then n
       else help  (n/ 10)  (i-1)
 



        let int2str n =
          let length = len n in
          string_init length (fun i -> 
            let d = ( (help (n)  (length-i-1)) mod 10) in  int_to_char d
          )



        