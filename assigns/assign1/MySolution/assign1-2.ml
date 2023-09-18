(* ****** ****** *)
(*
Assign1: Onward!

Total: 70 points + 20 bonus points
 
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
*)
(* ****** ****** *)

(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.






For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.

fun intrev10(n: int): int

*)

let chr = Char.chr
let ord = Char.code;;
let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in ord(ch) - ord('0')

    let char_to_int ch =
      Char.code ch
    

  let string_get_at(cs:string)(i0:int): char = String.get cs i0

  let string_init = String.init
  let string_length = String.length 
  let string_cons(c0: char)(cs: string): string = 
  string_init(string_length(cs) + 1)(
    fun i -> if i <= 0 then c0 else string_get_at cs (i-1))
  


let string_merge cs1 cs2 =
  let rec merge s1 s2 acc =
    match (s1, s2) with
    | "", "" -> acc
    | "", s2 -> s2
    | s1, "" -> s1
    | s1,s2->
                              
      if char_to_int (string_get_at s1 0) <= char_to_int (string_get_at s2 0)then
      string_cons  (string_get_at s1 0) (merge (String.sub s1 1 (String.length s1 - 1)) s2 acc)
      else
        string_cons  ( string_get_at s2 0 ) ( merge s1 (String.sub s2 1 (String.length s2 - 1)) acc)
  in
  merge cs1 cs2 "" ;;









      
