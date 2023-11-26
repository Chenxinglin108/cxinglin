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

let parse_value str =
  match str with
  | "True" -> Bool true
  | "False" -> Bool false
  | "Unit" -> Unit
  | _ -> Int (str2int str)

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
  | Trace, v :: s -> Ok (s, (string_of_value v) :: trace)
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
  let rec eval_commands cmds state =
    match cmds with
    | [] -> Ok state
    | cmd :: cmds' ->
        (match eval_command cmd state with
         | Ok state' -> eval_commands cmds' state'
         | Error e -> Error e)
  in
  match eval_commands commands ([], []) with
  | Ok (_, trace) -> Some  (trace)
  | Error _ -> Some (["Panic"])

let interp program_str =
  let commands = parse_program program_str in
  eval_program commands