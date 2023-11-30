(* Type definitions updated to include symbols and closures (functions) *)
#use "./../../../classlib/OCaml/MyOCaml.ml";;


type value = 
  | Int of int 
  | Bool of bool 
  | Unit 
  | Symbol of string 
  | Closure of string * (string * value) list * command list

(* Updated command type to include new commands *)
and command = 
  | Push of value 
  | Pop 
  | Trace 
  | Add 
  | Sub 
  | Mul 
  | Div 
  | And 
  | Or 
  | Not 
  | Lt 
  | Gt 
  | Swap 
  | IfElse of command list * command list 
  | Bind 
  | Lookup of string 
  | Fun of string * command list 
  | Call 
  | Return

(* Exception for error scenarios *)
exception Panic


let split_on_char delimiter str =
  let rec aux i j =
    if j >= string_length str then
      if i >= j then [] else [String.sub str i (j - i)]
    else if String.get str j = delimiter then
      if i = j then aux (j + 1) (j + 1)
      else String.sub str i (j - i) :: aux (j + 1) (j + 1)
    else aux i (j + 1)
  in
  aux 0 0

let string_get(cs, i0) = String.get cs i0




let
  str2int
  (cs: string): int =
  if string_get (cs,0) == '-' then let cs = String.sub cs 1 ((string_length cs)-1) in let result= let rec
    helper(i0: int): int =
        if i0 <= 0 then 0 else
        10 * helper(i0 - 1) + 
        ord(string_get(cs, i0-1)) - ord('0')in
        helper(string_length(cs)) in -1 * result
  else 
  let rec
  helper(i0: int): int =
      if i0 <= 0 then 0 else
      10 * helper(i0 - 1) + 
      ord(string_get(cs, i0-1)) - ord('0')in
      helper(string_length(cs))

      let is_valid_integer str =
        let str_len = string_length str in
        if str_len = 0 then false
        else
          let rec is_digit i =
            if i >= str_len then true
            else if i = 0 && string_get_at str i = '-' && str_len > 1 then is_digit (i + 1)
            else if string_get_at str i >= '0' && string_get_at str i <= '9' then is_digit (i + 1)
            else false
          in
          is_digit 0


      

      
let parse_value str =
  match str with
  | "True" -> Bool true
  | "False" -> Bool false
  | "Unit" -> Unit
  | _ ->Int (str2int str)

  let rec remove_empty_strings lst =
    match lst with 
    | [] -> []
    | "" :: t -> remove_empty_strings t
    | "\n" :: t -> remove_empty_strings t
    | h :: t -> h :: remove_empty_strings t

    let replace_newlines_with_space s =
      let rec aux i acc =
        if i >= string_length s then
          acc
        else if string_get_at s i = '\n' || string_get_at s i == '\t'then
          aux (i + 1) (string_append acc " ") 
        else
          aux (i + 1) (string_append acc (String.make 1 (string_get_at s i))) 
      in
      aux 0 ""

      let list_map(xs) = foreach_to_map_list(list_foreach)(xs)


      let rec parse_command str =
        let parts = remove_empty_strings (String.split_on_char ' ' (replace_newlines_with_space str)) in
        match parts with
        | ["Push"; v] -> Push (parse_value v)
        | ["Pop"] -> Pop
        | ["Trace"] -> Trace
        | ["Add"] -> Add
        | ["Sub"] -> Sub
        | ["Mul"] -> Mul
        | ["Div"] -> Div
        | ["And"] -> And
        | ["Or"] -> Or
        | ["Not"] -> Not
        | ["Lt"] -> Lt
        | ["Gt"] -> Gt
        | ["Swap"] -> Swap
        | ["IfElse"; then_cmds; else_cmds] -> IfElse (parse_program then_cmds, parse_program else_cmds)
        | ["Bind"] -> Bind
        | ["Lookup"; sym] -> Lookup sym
        | ["Fun"; name; body] -> Fun (name, parse_program body)
        | ["Call"] -> Call
        | ["Return"] -> Return
        | _ -> raise (Failure "Invalid command")
      and parse_program str =
        str 
        |> split_on_char ';' 
        |> (fun lst -> list_map lst String.trim)
        |> (fun lst -> list_foldright lst [] (fun s acc -> if s <> "" then s :: acc else acc))
        |> (fun lst -> list_map lst parse_command)
      
        
let parse_value str =
  match str with
  | "True" -> Bool true
  | "False" -> Bool false
  | "Unit" -> Unit
  | _ when str.[0] = '\'' -> Symbol (String.sub str 1 (String.length str - 1)) (* Symbols prefixed with ' *)
  | _ -> Int (str2int str)







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
    (*
    let
    _ = assert(x >= 0)
    in(*let*)
    *)
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
let string_of_value = function
  | Int i -> int2str i
  | Bool b -> if b == true then "True" else "False"
  | Unit -> "Unit"


  let rec eval_command cmd stack trace env =
    match cmd with
    | Push v -> Ok (v :: stack, trace, env)
    | Pop ->
        (match stack with
        | _ :: s -> Ok (s, trace, env)
        | _ -> Error Panic)
    | Trace ->
        (match stack with
        | v :: s -> Ok (Unit :: s, (string_of_value v) :: trace, env)
        | _ -> Error Panic)
    | Add ->
        (match stack with
        | Int a :: Int b :: s -> Ok (Int (a + b) :: s, trace, env)
        | _ -> Error Panic)
    | Sub ->
        (match stack with
        | Int a :: Int b :: s -> Ok (Int (a - b) :: s, trace, env)
        | _ -> Error Panic)
    | Mul ->
        (match stack with
        | Int a :: Int b :: s -> Ok (Int (a * b) :: s, trace, env)
        | _ -> Error Panic)
    | Div ->
        (match stack with
        | Int a :: Int b :: s ->
            if b = 0 then Error Panic
            else Ok (Int (a / b) :: s, trace, env)
        | _ -> Error Panic)
    | And ->
        (match stack with
        | Bool a :: Bool b :: s -> Ok (Bool (a && b) :: s, trace, env)
        | _ -> Error Panic)
    | Or ->
        (match stack with
        | Bool a :: Bool b :: s -> Ok (Bool (a || b) :: s, trace, env)
        | _ -> Error Panic)
    | Not ->
        (match stack with
        | Bool a :: s -> Ok (Bool (not a) :: s, trace, env)
        | _ -> Error Panic)
    | Lt ->
        (match stack with
        | Int a :: Int b :: s -> Ok (Bool (a < b) :: s, trace, env)
        | _ -> Error Panic)
    | Gt ->
        (match stack with
        | Int a :: Int b :: s -> Ok (Bool (a > b) :: s, trace, env)
        | _ -> Error Panic)
    | Swap ->
        (match stack with
        | a :: b :: s -> Ok (b :: a :: s, trace, env)
        | _ -> Error Panic)
    | IfElse (then_cmds, else_cmds) ->
        (match stack with
        | Bool b :: s ->
            if b then eval_program then_cmds (s, trace, env)
            else eval_program else_cmds (s, trace, env)
        | _ -> Error Panic)
    | Bind ->
        (match stack with
        | Symbol sym :: v :: s -> Ok (Unit :: s, trace, (sym, v) :: env)
        | _ -> Error Panic)
    | Lookup sym ->
        (match List.assoc_opt sym env with
        | Some v -> Ok (v :: stack, trace, env)
        | None -> Error Panic)
    | Fun (name, body) -> Ok (Closure (name, env, body) :: stack, trace, env)
    | Call ->
        (match stack with
        | Closure (_, closure_env, body) :: s -> eval_program body (s, trace, closure_env)
        | _ -> Error Panic)
    | Return ->
        (match stack with
        | v :: _ -> Ok ([v], trace, env)
        | _ -> Error Panic)
        and eval_program cmds (stack, trace, env) =
          let rec eval_program_helper cmds state =
            match cmds, state with
            | [], _ -> Ok state
            | cmd :: cmds', (stack, trace, env) ->
                match eval_command cmd stack trace env with
                | Ok (stack', trace', env') ->
                    eval_program_helper cmds' (stack', trace', env')
                | Error e -> Error e
          in
          eval_program_helper cmds (stack, trace, env)
          let interp program_str =
            if program_str = "" then Some [] else
              let commands = parse_program program_str in
              match eval_program commands ([], [], []) with
              | Ok (_, trace, _) -> Some trace
              | Error Panic -> Some (["Panic"])
          


(*
#use "./../../../classlib/OCaml/MyOCaml.ml";;

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type token =
(* Constants *)
| INT of int       (* Integer constants *)
| BOOL of bool     (* Boolean constants: True, False *)
| UNIT             (* Unit constant *)
| SYMBOL of string (* Symbol constants *)

(* Commands *)
| PUSH             (* Push command *)
| POP              (* Pop command *)
| SWAP             (* Swap command *)
| TRACE            (* Trace command *)

| ADD              (* Add command *)
| SUB              (* Subtract command *)
| MUL              (* Multiply command *)
| DIV              (* Divide command *)

| AND              (* Logical And command *)
| OR               (* Logical Or command *)
| NOT              (* Logical Not command *)

| LT               (* Less Than command *)
| GT               (* Greater Than command *)

| IF               (* If command *)
| ELSE             (* Else command *)
| END              (* End command *)

| BIND             (* Bind command *)
| LOOKUP           (* Lookup command *)

| FUN              (* Function declaration command *)
| CALL             (* Call command *)
| RETURN           (* Return command *)


(* Helper functions *)
let is_lower_case c = 'a' <= c && c <= 'z'
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_alpha c = is_lower_case c || is_upper_case c
let is_digit c = '0' <= c && c <= '9'
let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c
let is_blank c = String.contains " \012\n\r\t" c
let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)

(* Matching functions *)

let match_int cs =
let rec loop cs =
  match cs with
  | c :: cs ->
    if is_digit c then
      let digits, cs = loop cs in
      (c :: digits, cs)
    else
      ([], c :: cs)
  | [] -> ([], [])
in
match cs with
| '-' :: cs -> (
    match loop cs with
    | digits, rest when digits <> [] -> Some (INT (int_of_string (implode ('-' :: digits))), rest)
    | _ -> None)
| cs -> (
    match loop cs with
    | digits, rest when digits <> [] -> Some (INT (int_of_string (implode digits)), rest)
    | _ -> None)

let match_bool cs =
match cs with
| 'T' :: 'r' :: 'u' :: 'e' :: rest -> Some (BOOL true, rest)
| 'F' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> Some (BOOL false, rest)
| _ -> None

let match_unit cs =
match cs with
| 'U' :: 'n' :: 'i' :: 't' :: rest -> Some (UNIT, rest)
| _ -> None

let match_symbol cs =
let rec loop acc cs =
  match cs with
  | c :: cs when is_alpha c || is_digit c || c = '_' -> loop (c :: acc) cs
  | _ -> (acc, cs)
in
match loop [] cs with
| [], _ -> None
| acc, rest -> Some (SYMBOL (implode (List.rev acc)), rest)

let match_commands = function
| 'P' :: 'u' :: 's' :: 'h' :: rest -> Some (PUSH, rest)
| 'P' :: 'o' :: 'p' :: rest -> Some (POP, rest)
| 'S' :: 'w' :: 'a' :: 'p' :: rest -> Some (SWAP, rest)
| 'T' :: 'r' :: 'a' :: 'c' :: 'e' :: rest -> Some (TRACE, rest)
| 'A' :: 'd' :: 'd' :: rest -> Some (ADD, rest)
| 'S' :: 'u' :: 'b' :: rest -> Some (SUB, rest)
| 'M' :: 'u' :: 'l' :: rest -> Some (MUL, rest)
| 'D' :: 'i' :: 'v' :: rest -> Some (DIV, rest)
| 'A' :: 'n' :: 'd' :: rest -> Some (AND, rest)
| 'O' :: 'r' :: rest -> Some (OR, rest)
| 'N' :: 'o' :: 't' :: rest -> Some (NOT, rest)
| 'L' :: 't' :: rest -> Some (LT, rest)
| 'G' :: 't' :: rest -> Some (GT, rest)
| 'I' :: 'f' :: rest -> Some (IF, rest)
| 'E' :: 'l' :: 's' :: 'e' :: rest -> Some (ELSE, rest)
| 'E' :: 'n' :: 'd' :: rest -> Some (END, rest)
| 'B' :: 'i' :: 'n' :: 'd' :: rest -> Some (BIND, rest)
| 'L' :: 'o' :: 'o' :: 'k' :: 'u' :: 'p' :: rest -> Some (LOOKUP, rest)
| 'F' :: 'u' :: 'n' :: rest -> Some (FUN, rest)
| 'C' :: 'a' :: 'l' :: 'l' :: rest -> Some (CALL, rest)
| 'R' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: rest -> Some (RETURN, rest)
| _ -> None

(* Main Lexer Function *)
let lexer s =
let rec aux cs =
  match cs with
  | [] -> []
  | c :: cs when is_blank c -> aux cs
  | cs ->
    match match_int cs with
    | Some (token, rest) -> token :: aux rest
    | None ->
      match match_bool cs with
      | Some (token, rest) -> token :: aux rest
      | None ->
        match match_unit cs with
        | Some (token, rest) -> token :: aux rest
        | None ->
          match match_symbol cs with
          | Some (token, rest) -> token :: aux rest
          | None ->
            match match_commands cs with
            | Some (token, rest) -> token :: aux rest
            | None -> failwith "Lexer error: unexpected character sequence"
in
aux (explode s)



type const =
| Int of int
| Bool of bool
| Unit
| Symbol of string

type com =
| Push of const
| Pop
| Swap
| Trace
| Add
| Sub
| Mul
| Div
| And
| Or
| Not
| Lt
| Gt
| If of prog * prog
| Bind
| Lookup
| Fun of prog
| Call
| Return
and prog = com list

(* Parse constants *)
let parse_const = function
| INT i :: rest -> Some (Int i, rest)
| BOOL b :: rest -> Some (Bool b, rest)
| UNIT :: rest -> Some (Unit, rest)
| SYMBOL s :: rest -> Some (Symbol s, rest)
| _ -> None

(* Parse individual commands *)
let rec parse_command tokens =
match tokens with
| PUSH :: rest ->
  (match parse_const rest with
  | Some (const, rest') -> Some (Push const, rest')
  | None -> None)
| POP :: rest -> Some (Pop, rest)
| SWAP :: rest -> Some (Swap, rest)
| TRACE :: rest -> Some (Trace, rest)
| ADD :: rest -> Some (Add, rest)
| SUB :: rest -> Some (Sub, rest)
| MUL :: rest -> Some (Mul, rest)
| DIV :: rest -> Some (Div, rest)
| AND :: rest -> Some (And, rest)
| OR :: rest -> Some (Or, rest)
| NOT :: rest -> Some (Not, rest)
| LT :: rest -> Some (Lt, rest)
| GT :: rest -> Some (Gt, rest)
| IF :: rest -> 
  (match parse_prog rest with
  | Some (cmds1, ELSE :: rest') ->
    (match parse_prog rest' with
    | Some (cmds2, END :: rest'') -> Some (If (cmds1, cmds2), rest'')
    | _ -> None)
  | _ -> None)
| BIND :: rest -> Some (Bind, rest)
| LOOKUP :: rest -> Some (Lookup, rest)
| FUN :: rest ->
  (match parse_prog rest with
  | Some (cmds, END :: rest') -> Some (Fun cmds, rest')
  | _ -> None)
| CALL :: rest -> Some (Call, rest)
| RETURN :: rest -> Some (Return, rest)
| _ -> None

and parse_prog tokens =
 let rec aux acc toks = match toks with
   | [] -> Some (List.rev acc, [])  (* Successfully parsed all tokens *)
   | _ ->
     match parse_command toks with
     | Some (com, rest) -> aux (com :: acc) rest
     | None -> None  (* Encountered an unparseable token *)
 in
 aux [] tokens

(* Main parsing function *)
let parse tokens =
 match parse_prog tokens with
 | Some (prog, []) -> Some prog  (* Successfully parsed the whole program and no remaining tokens *)
 | Some (_, _) | None -> None

 type value =
 | IntVal of int
 | BoolVal of bool
 | UnitVal
 | SymbolVal of string

(* Convert const to value *)
let value_of_const = function
 | Int i -> IntVal i
 | Bool b -> BoolVal b
 | Unit -> UnitVal
 | Symbol s -> SymbolVal s

(* String Representation of Value *)
let rec string_of_value = function
 | IntVal i -> string_of_int i
 | BoolVal b -> string_of_bool b
 | UnitVal -> "unit"
 | SymbolVal s -> s


(* Implement the eval function to process each command *)
let rec eval coms stack =
 match coms with
 | [] -> Some stack  (* End of program *)
 | com :: coms -> 
   match com with
   | Push c -> eval coms ((value_of_const c) :: stack)  (* Convert const to value *)
   | Pop -> 
     (match stack with
     | _ :: stack' -> eval coms stack'
     | [] -> None)  (* Stack underflow *)
   | Swap -> 
     (match stack with
     | a :: b :: stack' -> eval coms (b :: a :: stack')
     | _ -> None)  (* Insufficient elements for Swap *)
   | Add -> 
     (match stack with
     | IntVal i1 :: IntVal i2 :: stack' -> eval coms (IntVal (i1 + i2) :: stack')
     | _ -> None)
   | Sub -> 
     (match stack with
     | IntVal i1 :: IntVal i2 :: stack' -> eval coms (IntVal (i1 - i2) :: stack')
     | _ -> None)
   | Mul -> 
     (match stack with
     | IntVal i1 :: IntVal i2 :: stack' -> eval coms (IntVal (i1 * i2) :: stack')
     | _ -> None)
   | Div -> 
     (match stack with
     | IntVal i1 :: IntVal i2 :: stack' when i2 != 0 -> eval coms (IntVal (i1 / i2) :: stack')
     | _ -> None)
   | And -> 
     (match stack with
     | BoolVal b1 :: BoolVal b2 :: stack' -> eval coms (BoolVal (b1 && b2) :: stack')
     | _ -> None)
   | Or -> 
     (match stack with
     | BoolVal b1 :: BoolVal b2 :: stack' -> eval coms (BoolVal (b1 || b2) :: stack')
     | _ -> None)
   | Not -> 
     (match stack with
     | BoolVal b :: stack' -> eval coms (BoolVal (not b) :: stack')
     | _ -> None)
   | If (prog1, prog2) -> 
     (match stack with
     | BoolVal b :: stack' -> 
       if b then eval (prog1 @ coms) stack'
       else eval (prog2 @ coms) stack'
     | _ -> None)
   (* Add other commands like Lt, Gt, Bind, Lookup, etc. *)
   | _ -> None  (* Other commands not implemented yet *)

(* Entry point of the interpreter *)
let interp (s : string) : string list option =
 match parse (lexer s) with
 | Some prog ->
   (match eval prog [] with
   | Some stack -> Some (List.map string_of_value stack)
   | None -> None)
 | None -> None
