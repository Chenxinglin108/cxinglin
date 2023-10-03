let rec helper (f: 'a -> 'b) (ls: 'a list) : 'b list = 
  match ls with
  | [] -> []
  | h::t -> f h :: helper f t

  

let get_h (ls : 'a list) : 'a = 
  match ls with
  | [] -> failwith "hd"
  | h::t -> h


let get_t (ls : 'a list) : 'a list =
  match ls with
  | [] -> failwith "tl"
  | h::t -> t


  let rec
matrix_transpose(xss: 'a list list): 'a list list=
  match xss with
  | [] -> []
  | [] :: tl  -> matrix_transpose tl
  | (hh :: tt) :: t -> 
    (hh :: helper get_h t) :: matrix_transpose (tt :: helper get_t t)


