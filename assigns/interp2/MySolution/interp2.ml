
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
          
