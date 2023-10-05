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







    type 'a strcon =
    StrNil
  | StrCons of
    'a * (unit -> 'a strcon)
  
  (* ****** ****** *)
  
  type 'a stream =
  unit -> 'a strcon (* thunk *)
  
  (* ****** ****** *)
  
  let rec
  stream_from(n: int) =
  fun () -> (* thunk *)
  StrCons(n, stream_from(n+1))
  
  
  let theNats = stream_from(0)
  
  (*
  let fxs = theNats
  let StrCons(x0, fxs) = fxs()
  let StrCons(x1, fxs) = fxs()
  let StrCons(x2, fxs) = fxs()
  ;;
  *)
  
  (*
   2 :: 3 :: 5 ::
   7 :: 11 :: 13 :: [...]
  *)
  let rec
  sieve
  (fxs: int stream): int stream = fun() ->
  match fxs() with
  |
  StrNil -> failwith "whatever!!!"
  |
  StrCons(p1, fxs) ->
  let rec
  filter(fxs) = fun() ->
  match fxs() with
  |
  StrNil -> failwith "whatever!!!"
  |
  StrCons(x1, fxs) ->
  if x1 mod p1 = 0
  then filter(fxs)()
  else StrCons(x1, filter(fxs))
  in
    StrCons(p1, sieve(filter(fxs)))
  
  let thePrimes = sieve(stream_from(2))
  