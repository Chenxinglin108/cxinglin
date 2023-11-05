


#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)



let parse_digit cs =
  match cs with
  | c :: cs' when '0' <= c && c <= '9' -> Some (ord c - ord '0', cs')
  | _ -> None


let rec parse_num cs =
    match parse_digit cs with
    | Some (d, cs') ->

         (match parse_num cs' with
         | Some (n, cs'') -> Some (10 * d + n, cs'')
         | None -> Some (d, cs'))

    | None -> None





let rec parse_expr cs =
  let cs = trim cs in
  match cs with
  | '(' :: 'a' :: 'd' :: 'd' :: cs' -> 
      let rec parse_add_exprs cs =
        match parse_expr cs with
        | Some (e, cs') ->
            begin
              match parse_add_exprs cs' with
              | Some (es, cs'') -> Some (e :: es, cs'')
              | None -> Some ([e], cs')
            end
        | None -> None
      in
      begin
        match parse_add_exprs cs' with
        | Some (es, ')' :: cs'') -> Some (Add es, cs'')
        | _ -> None
      end

  | '(' :: 'm' :: 'u' :: 'l' :: cs' -> 
      let rec parse_mul_exprs cs =
        match parse_expr cs with
        | Some (e, cs') ->
            begin
              match parse_mul_exprs cs' with
              | Some (es, cs'') -> Some (e :: es, cs'')
              | None -> Some ([e], cs')
            end
        | None -> None
      in
      begin
        match parse_mul_exprs cs' with
        | Some (es, ')' :: cs'') -> Some (Mul es, cs'')
        | _ -> None
      end

  | _ -> 
      match parse_num cs with
      | Some (n, cs') -> Some (Int n, cs')
      | None -> None




      let parse (s : string) : expr option =
        match parse_expr (string_listize s) with
        | Some (e, []) -> Some e
        | _ -> None