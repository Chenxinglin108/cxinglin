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
type command = Push of value | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt

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
        let str_len = String.length str in
        if str_len = 0 then false
        else
          let rec is_digit i =
            if i >= str_len then true
            else if i = 0 && str.[i] = '-' && str_len > 1 then is_digit (i + 1)
            else if str.[i] >= '0' && str.[i] <= '9' then is_digit (i + 1)
            else false
          in
          is_digit 0


      

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
        let str_len = String.length str in
        if str_len = 0 then false
        else
          let rec is_digit i =
            if i >= str_len then true
            else if i = 0 && str.[i] = '-' && str_len > 1 then is_digit (i + 1)
            else if str.[i] >= '0' && str.[i] <= '9' then is_digit (i + 1)
            else false
          in
          is_digit 0
      
let parse_value str =
  match str with
  | "True" -> Bool true
  | "False" -> Bool false
  | "Unit" -> Unit
  | _ ->Int (str2int str)


let parse_command str =
  let parts = String.split_on_char ' ' str in
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
    |> String.split_on_char ';' 
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

let eval_command cmd (stack, trace) =
  match cmd, stack with
  | Push v, _ -> Ok (v :: stack, trace)
  | Pop, _ :: s -> Ok (s, trace)
  | Trace, v :: s -> Ok (Unit :: s, (string_of_value v) :: trace)
  | Add, (Int a) :: (Int b) :: s -> Ok (Int (a + b) :: s, trace)
  | Sub, (Int a) :: (Int b) :: s -> Ok (Int (b - a) :: s, trace)
  | Mul, (Int a) :: (Int b) :: s -> Ok (Int (a * b) :: s, trace)
  | Div, (Int a) :: (Int b) :: s -> 
      if b = 0 then Error Panic 
      else Ok (Int (a / b) :: s, trace)
  | And, (Bool a) :: (Bool b) :: s -> Ok (Bool (a && b) :: s, trace)
  | Or, (Bool a) :: (Bool b) :: s -> Ok (Bool (a || b) :: s, trace)
  | Not, (Bool a) :: s -> Ok (Bool (not a) :: s, trace)
  | Lt, (Int a) :: (Int b) :: s -> Ok (Bool (a < b) :: s, trace)
  | Gt, (Int a) :: (Int b) :: s -> Ok (Bool (a > b) :: s, trace)
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
        if String.length cmd > 4 && String.sub cmd 0 4 = "Push" then
          let rest = String.trim (String.sub cmd 4 (String.length cmd - 4)) in
          is_valid_integer rest || rest = "True" || rest = "False" ||rest= "Unit"
        else
          false
    
  
  let are_all_valid_commands cmd_list =
   list_foldleft cmd_list true(fun acc cmd -> acc && is_valid_command cmd) 

   let interp program_str = if program_str == "" then Some [] else if (are_all_valid_commands(clean_string_list(String.split_on_char ';' program_str))) == true then
    let commands = parse_program program_str in
    eval_program commands else None
   