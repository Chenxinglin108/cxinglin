(* ****** ****** *)
(*
//
Assign6:
Parsing and parsing combinators
//
DUE: the 13th of November, 2023
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])
//
Example (Rejected Strings):
parse "()" = None
parse "(add)" = None
parse "(add 1 2))" = None
parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)



(* ****** ****** *)

(* end of [CS320-2023-Fall-assigns-assign6.ml] *)
(* Character predicates and utilities *)



#use "./../../classlib/OCaml/MyOCaml.ml"



let
strapp
((xs: string)
,(ys: string)) =
let m =
string_length(xs) in
let n =
string_length(ys) in
string_init (m + n)
(fun i ->
 if i < m
 then string_get(xs, i) else string_get(ys, i-m))

(* ****** ****** *)

let rec
nat2str
(x: int): string =

if x < 10
then
str(chr((ord('0') + x mod 10)))
else
strapp
(nat2str(x / 10), str(chr((ord('0') + x mod 10))))
(* end-of-let *)

(* ****** ****** *)

let rec
int2str(x: int) =
if x >= 0 then nat2str(x) else str('-') ^ nat2str(-x)

type sexpr =
| SInt of int
| SAdd of sexpr list
| SMul of sexpr list

let list_map(xs) = foreach_to_map_list(list_foreach)(xs)
let ws : unit parser =
  many (char ' ') >| ()

let rec expr () =
  ws >>= fun _ ->
  (natural >>= fun n -> pure (SInt n)) <|>
  (char '(' >> 
    ws >>
    ((keyword "add" >> pure (fun l -> SAdd l)) <|>
    (keyword "mul" >> pure (fun l -> SMul l))) >>= fun constructor ->
    many (expr ()) << char ')' << ws >>= fun exprs ->
    pure (constructor exprs))


let rec sexpr_parse (s: string) : sexpr option =
  match string_parse (expr ()) s with
  | Some (sexpr, []) -> Some sexpr
  | _ -> None


  let concat sep lst =
    match lst with
    | [] -> ""
    | hd :: tl ->
     list_foldleft tl hd (fun acc x -> string_append (string_append acc sep) x)
  
  let rec sexpr_to_string e =
    match e with
    | SInt n -> int2str n
    | SAdd exprs -> string_append (string_append "(add " (concat " " (list_map exprs sexpr_to_string))) ")"
    | SMul exprs -> string_append (string_append "(mul " (concat " " (list_map exprs sexpr_to_string))) ")"
