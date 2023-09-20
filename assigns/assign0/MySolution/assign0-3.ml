

(*Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

#use "./../assign0.ml";;



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
      if n = 0 then 1 else
      let rec helper i c=
        if i =0 then c
        else  helper (i/10) (c+1)
       in helper n 0
   

       let rec help n i=
       if i=0 || i<0 then n
       else help  (n/ 10)  (i-1)
 

        let int2str n =
          if n=0 then "0" else
          let length = len n in
          string_init length (fun i -> 
            let d = ( (help (n)  (length-i-1)) mod 10) in  int_to_char d
          )



        