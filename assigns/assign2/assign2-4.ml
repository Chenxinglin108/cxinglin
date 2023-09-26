(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"


*)


let string_length = String.length
let string_init = String.init

let string_get_at(cs:string)(i0:int): char = String.get cs i0


let string_cons(c0: char)(cs: string): string = 
  string_init(string_length(cs) + 1)(
    fun i -> if i <= 0 then c0 else string_get_at cs (i-1)
  )





let string_sepjoin_list (sep: string) (xs: string list): string =
  let concat s1 s2 = if s1 = "" then s2 else string_append (string_append (s1) (sep))  (s2) in
  list_foldleft xs "" concat 