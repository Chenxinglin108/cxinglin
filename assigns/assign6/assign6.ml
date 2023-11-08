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

type sexpr =
  | SInt of int
  | SAdd of sexpr list
  | SMul of sexpr list




type 'a parser = char list -> ('a * char list) option

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None
let (>>=) = bind
let (let*) = bind

(* Character predicates and utilities *)
let is_digit c =
  '0' <= c && c <= '9'

let implode ls =
  String.of_seq (List.to_seq ls)

(* Parser combinators *)
let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (List.of_seq (String.to_seq s))

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun _ -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None

let (>>=) = bind

let char (c : char) : char parser =
  fun ls ->
    match ls with
    | hd :: tl when hd = c -> Some (c, tl)
    | _ -> None

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
    match p ls with
    | Some (x, ls) ->
      (match many1 p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([x], ls))
    | None -> None


    let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
      fun ls ->
      match p1 ls with
      | Some (_, ls) -> p2 ls
      | None -> None
    
    let (>>) = seq
    
    let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
      fun ls ->
      match p1 ls with
      | Some (x, ls) ->
        (match p2 ls with
         | Some (_, ls) -> Some (x, ls)
         | None -> None)
      | None -> None
    
    let (<<) = seq'  


let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = disj

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)
let rec many (p : 'a parser) : ('a list) parser =
  many1 p <|> pure []

let ws : unit parser =
  many (char ' ') >| ()

let keyword (kw : string) : unit parser =
  let chars = List.of_seq (String.to_seq kw) in
  let rec consume_chars chars ls =
    match chars, ls with
    | [], ls -> Some ((), ls)
    | _, c :: rest when c = List.hd chars ->
      consume_chars (List.tl chars) rest
    | _ -> None
  in
  consume_chars chars >>= fun _ -> ws

let digit : int parser =
  fun ls ->
    match ls with
    | c :: rest when is_digit c ->
      let num = int_of_char c - int_of_char '0' in
      Some (num, rest)
    | _ -> None

let natural : int parser =
  many1 digit >|= fun digits ->
  List.fold_left (fun acc d -> acc * 10 + d) 0 digits

  type sexpr =
  | SInt of int
  | SAdd of sexpr list
  | SMul of sexpr list

type 'a parser = char list -> ('a * char list) option


let rec expr () =
  ws >>= fun _ ->
  (natural >>= fun n -> pure (SInt n)) <|>
  (char '(' >> 
    ws >>
    ((keyword "add" >> pure (fun l -> SAdd l)) <|>
    (keyword "mul" >> pure (fun l -> SMul l))) >>= fun constructor ->
    many (expr ()) << char ')' << ws >>= fun exprs ->
    pure (constructor exprs))

let parse_expr s =
  match parse (expr ()) s with
  | Some (sexpr, []) -> Some sexpr
  | _ -> None

let rec parse_expr (s: string) : sexpr option =
  match parse (expr ()) s with
  | Some (sexpr, []) -> Some sexpr
  | _ -> None





  let rec sexpr_to_string e =
    match e with
    | SInt n -> string_of_int n
    | SAdd exprs -> "(add " ^ (String.concat " " (List.map sexpr_to_string exprs)) ^ ")"
    | SMul exprs -> "(mul " ^ (String.concat " " (List.map sexpr_to_string exprs)) ^ ")"

let example0 = "(1 2 3)" 


let example1 = "(add 1 2 3)" (* Should return Some (SAdd [SInt 1; SInt 2; SInt 3]) *)


let example2 = "(mul (add 1 2) 3 (mul 1))" (* Should return Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]]) *)


let result0 = parse_expr example0




let result1 = parse_expr example1
let result2 = parse_expr example2


