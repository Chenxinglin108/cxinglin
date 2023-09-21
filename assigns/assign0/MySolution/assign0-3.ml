

(*Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

#use "./../assign0.ml";;



let int_to_char n =
  if n = 0 then '0'
  else if n = 1 then '1'
  else if n = 2 then '2'
  else if n = 3 then '3'
  else if n = 4 then '4'
  else if n = 5 then '5'
  else if n = 6 then '6'
  else if n = 7 then '7'
  else if n = 8 then '8'
  else  '9'




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



        