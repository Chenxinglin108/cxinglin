#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* abstract syntax tree of interp1 *)

type symbol = string
type character = char

type const =
  | Int of int
  | Bool of bool
  | Unit 
  | Sym of symbol
  

type com =
  | Push of const | Pop | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt| Swap
  | Bind | Lookup
  | Fun of coms | Call | Return

type coms = com list


let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))


  let list_map(xs) = foreach_to_map_list(list_foreach)(xs)
  let parse_unit =
  keyword "Unit" >> pure Unit

  let parse_sym =
    let parse_char_digit =
      satisfy (fun c -> (c >= 'a' && c <= 'z')  || (c >= '0' && c <= '9'))
    in
    many1' (fun () -> parse_char_digit) >>= fun chars ->
    pure (Sym (string_concat_list (list_map chars (String.make 1))))
  
let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_sym

let parse_com = 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt)<|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) 

let parse_coms = many (parse_com << keyword ";")



type stack = const list
type trace = string list
type prog = coms
type env = (string * const) list

type closure = {
  cl_name: string;          (* Function name *)
  cl_env: (string * const) list;  (* Closure environment *)
  cl_body: prog;            (* Closure body *)
}

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Sym s -> s 


let rec eval (s : stack) (t : trace) (p : prog) (e:env) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t p0 e

  (*| Push c :: p0 -> (* PushStack *)
    let actual_value = match c with
      | Sym name -> (match List.assoc_opt name e with
                      | Some value -> value
                      | None -> c) (* If symbol not in env, push symbol itself *)
      | _ -> c
    in eval (actual_value :: s) t p0 e *)
  | Pop :: p0 ->
    (match s with
     | _ :: s0 (* PopStack *) -> eval s0 t p0 e
     | []      (* PopError *) -> eval [] ("Panic" :: t) [] e )
  | Trace :: p0 ->
    (match s with
     | c :: s0 (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) p0 e
     | []      (* TraceError *) -> eval [] ("Panic" :: t) [] e)
  | Add :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t p0 e
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) [] e
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) [] e)
  | Sub :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t p0 e
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) [] e
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) [] e)
  | Mul :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t p0 e
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) [] e
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) [] e)
  | Div :: p0 ->
    (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) [] e
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t p0 e
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) [] e
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) [] e)
  | And :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t p0 e
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) [] e
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) [] e)
  | Or :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t p0 e
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) [] e
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) [] e)
  | Not :: p0 ->
    (match s with
     | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t p0 e
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) [] e
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) [] e)
  | Lt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t p0 e
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) [] e
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) [] e)
  | Gt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t p0 e
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) [] e
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) [] e
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) [] e)
   | Swap :: p0 ->
     (match s with
       | i ::  j :: s0 (* GtStack *)  -> eval (j:: i :: s0) t p0 e
     
       | []                   (* GtError2 *) -> eval [] ("Panic" :: t) [] e
       | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) [] e)

    | Bind :: p0 -> (* Handle Bind *)
      (match s with
        | Sym x :: v :: s0 -> eval s0 t p0 ((x, v) :: e)  (* Successful bind *)
        | Sym x :: [] -> eval [] ("Panic" :: t) [] e      (* BindError3: Only one element on the stack *)
        | [] -> eval [] ("Panic" :: t) [] e               (* BindError2: Empty stack *)
        | _ -> eval [] ("Panic" :: t) [] e)  

    | Lookup :: p0 ->
      (match s with
        | Sym x :: s0 -> 
             (match List.assoc_opt x e with
              | Some v -> eval (v :: s0) t p0 e  (* Successful lookup *)
              | None -> eval [] ("Panic" :: t) [] e)  (* LookupError3: Symbol not bound *)
           | [] -> eval [] ("Panic" :: t) [] e  (* LookupError2: Stack is empty *)
           | _ -> eval [] ("Panic" :: t) [] e)  (* LookupError1: Top of stack is not a symbol *)





 
let initial_env : env = []  (* This would contain any predefined bindings *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms) s with
  | Some (p, []) -> 
    let initial_stack = [] (* Starting with an empty stack *) in
    let initial_trace = [] (* Starting with an empty trace *) in
    let final_trace = eval initial_stack initial_trace p initial_env in
    Some final_trace (* Return the final trace after evaluation *)
  | Some (_, _ :: _) -> 
    None (* Parsing did not consume the entire input, which indicates an error *)
  | None -> 
    None (* Parsing failed, likely due to a syntax error in the input program *)
      

