#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type ('a, 'e) result = Ok of 'a | Error of 'e
type value = Int of int | Bool of bool | Unit 
type value = 
  | Int of int 
  | Bool of bool 
  | Unit 
  | Closure of string * command list * (string * value) list  (* New type for closures *)
  | Symbol of string  (* New type for symbols *)

type command = 
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
  | Swap  (* New command *)
  | Bind  (* New command *)
  | Lookup (* New command *)
  | Fun of string * command list  (* New command for function definition *)
  | Call  (* New command for function call *)
  | Return  (* New command for returning from a function *)
  | IfElse of command list * command list  (* New command for conditional execution *)






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
    

    
    
let parse_command str =
  let parts = remove_empty_strings(split_on_char ' ' (replace_newlines_with_space(str))) in
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
  | _ -> raise (Failure "Invalid command")

  let list_map(xs) = foreach_to_map_list(list_foreach)(xs)
  let parse_program str =
    str 
    |> split_on_char ';' 
    |> (fun lst -> list_map lst String.trim)
    |> (fun lst -> list_foldright lst [] (fun s acc -> if s <> "" then s :: acc else acc))
    |> (fun lst -> list_map lst parse_command)
  


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

  let eval_command cmd (stack, env, trace) =
    match cmd, stack with
    | Push v, _ -> Ok (v :: stack, env, trace)
    | Pop, _ :: s -> Ok (s, env, trace)
    | Trace, v :: s -> Ok (Unit :: s, env, (string_of_value v) :: trace)
    | Add, (Int a) :: (Int b) :: s -> Ok (Int (a + b) :: s, env, trace)
    | Sub, (Int a) :: (Int b) :: s -> Ok (Int (a - b) :: s, env, trace)
    | Mul, (Int a) :: (Int b) :: s -> Ok (Int (a * b) :: s, env, trace)
    | Div, (Int a) :: (Int b) :: s -> 
        if b = 0 then Error Panic 
        else Ok (Int (a / b) :: s, env, trace)
    | And, (Bool a) :: (Bool b) :: s -> Ok (Bool (a && b) :: s, env, trace)
    | Or, (Bool a) :: (Bool b) :: s -> Ok (Bool (a || b) :: s, env, trace)
    | Not, (Bool a) :: s -> Ok (Bool (not a) :: s, env, trace)
    | Lt, (Int a) :: (Int b) :: s -> Ok (Bool (a < b) :: s, env, trace)
    | Gt, (Int a) :: (Int b) :: s -> Ok (Bool (a > b) :: s, env, trace)
    | Swap, a :: b :: s -> Ok (b :: a :: s, env, trace)
    | Bind, (Symbol sym) :: v :: s -> Ok (s, (sym, v) :: env, trace)
    | Lookup, (Symbol sym) :: s -> 
        (match List.assoc_opt sym env with
         | Some v -> Ok (v :: s, env, trace)
         | None -> Error Panic)
    | Fun(sym, cmds), s -> Ok (Closure(sym, cmds, env) :: s, env, trace)
    | Call, (Closure(sym, cmds, closure_env)) :: s -> 
      let new_env = (sym, args) :: closure_env in
      (* Execute the function's commands in the new environment *)
      (* Note: You'll need to modify this to suit your language's semantics *)
      Ok (args, new_env, trace @ cmds)
    | Return, v :: s -> 
      Ok (v :: s, env, trace)
    | IfElse(true_cmds, false_cmds), (Bool b) :: s ->
          if b then Ok (s, env, trace @ true_cmds)
          else Ok (s, env, trace @ false_cmds)
      | IfElse(_, _), (Bool _) :: _ -> Error ("Panic", env, trace)  (* IfElseError1: b is not a boolean *)
      | IfElse(_, _), [] -> Error ("Panic", env, trace)  (* IfElseError2: Stack is empty *)
    | _ -> Error Panic
  
  let eval_program commands =
    let rec eval_commands cmds (stack, trace) =
      match cmds with
      | [] -> Ok (stack, trace) 
      | cmd :: cmds' ->
          match eval_command cmd (stack, trace) with
          | Ok state' -> eval_commands cmds' state' 
          | Error _ -> Error (stack, "Panic" :: trace) 
    in
    match eval_commands commands ([], []) with
    | Ok (_, trace) -> Some (trace) 
    | Error (_, trace) -> Some (trace)  
  



  let remove_whitespaces str =
    let is_whitespace c = 
      c = ' ' || c = '\n' || c = '\r' || c = '\t'
    in 
    list_foldleft (String.to_seq str |> List.of_seq) "" (fun acc c ->
      if is_whitespace c then acc else acc ^ String.make 1 c
    ) 
  
  let clean_string_list str_list =
    list_map  str_list remove_whitespaces
  
  

    let is_valid_command cmd =
      let cmd = String.trim cmd in
      match cmd with
      ""->true
      | "Pop" | "Trace" | "Add" | "Sub" | "Mul" | "Div" | "And" | "Or" | "Not" | "Lt" | "Gt"  -> true
      | _ ->
        if string_length cmd > 4 && String.sub cmd 0 4 = "Push" then
          let rest = String.trim (String.sub cmd 4 (string_length cmd - 4)) in
          is_valid_integer rest || rest = "True" || rest = "False" ||rest= "Unit"
        else
          false
    
  
  let are_all_valid_commands cmd_list =
   list_foldleft cmd_list true(fun acc cmd -> acc && is_valid_command cmd) 

   let interp program_str = if program_str == "" then Some [] else if (are_all_valid_commands(clean_string_list(split_on_char ';' program_str))) == true then
    let commands = parse_program program_str in
    eval_program commands else None
   