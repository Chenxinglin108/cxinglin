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

type closure = {
  cl_name: string;          (* Function name *)
  cl_env: (string * const) list;  (* Closure environment *)
  cl_body: prog;            (* Closure body *)
}

and const =
  | Int of int
  | Bool of bool
  | Unit 
  | Sym of symbol
  | Closure of closure
  

and com =
  | Push of const | Pop | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt| Swap
  | Bind | Lookup
  | IfElse of coms * coms
  | Fun of coms | Call | Return

and coms = com list

and prog = coms

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


  let parse_sy =
    let parse_char_digit =
      satisfy (fun c -> (c >= 'a' && c <= 'z')  || (c >= '0' && c <= '9'))
    in
    many1' (fun () -> parse_char_digit) >>= fun chars ->
    pure (Sym (string_concat_list (list_map chars (String.make 1))))


      let parse_sym =
        let parse_char_digit =
          satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
        in
        many1' (fun () -> parse_char_digit) >>= fun chars ->
        let symbol_name = String.concat "" (List.map (String.make 1) chars) in
        pure (Sym symbol_name)  (* Wraps the string as a symbol constant *)
      
  
let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_sym


  
  let rec parse_com ()= 
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
    (keyword "Gt" >> pure Gt) <|>
    (keyword "Swap" >> pure Swap) <|>
    (keyword "Bind" >> pure Bind) <|>
    (keyword "Lookup" >> pure Lookup) <|>
    (keyword "Return" >> pure Return) <|>
    (keyword "Call" >> pure Call) <|>
    parse_ifelsecom ()  <|>
parse_funcom ()    


and parse_ifelsecom ()=
let* _ = keyword "If" in
let* c1 = parse_coms() in
let* _ = keyword "Else" in
let* c2 = parse_coms ()in
let* _ = keyword "End" in
pure (IfElse (c1, c2))
and parse_funcom () =
  let* _ = keyword "Fun" in
  let* body = parse_coms () in
  let* _ = keyword "End" in
  pure (Fun (body))


    and parse_coms() = many (parse_com() << keyword ";")

type stack = const list
type trace = string list

type env = (string * const) list

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
  | Closure cl -> "Fun<" ^ cl.cl_name ^ ">"


  let assoc_opt key lst =
    list_foldleft lst None (fun acc (k,v) -> if k = key then Some v else acc) 


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
             (match assoc_opt x e with
              | Some v -> eval (v :: s0) t p0 e  (* Successful lookup *)
              | None -> eval [] ("Panic" :: t) [] e)  (* LookupError3: Symbol not bound *)
           | [] -> eval [] ("Panic" :: t) [] e  (* LookupError2: Stack is empty *)
           | _ -> eval [] ("Panic" :: t) [] e)  (* LookupError1: Top of stack is not a symbol *)

    |IfElse (if_coms, else_coms) :: rest ->
            (match s with
             | Bool b :: s' -> 
                if b then eval s' t (if_coms @ rest) e (* Execute 'if' block *)
                else eval s' t (else_coms @ rest) e (* Execute 'else' block *)
             | [] -> eval [] ("Panic" :: t) [] e (* IfElseError2: Stack is empty *)
             | _ -> eval [] ("Panic" :: t) [] e) (* IfElseError1: Top of stack is not a boolean *)

      | Fun (body) :: rest -> (
        match s with
        | Sym x :: s' ->
            let closure = { cl_name = x; cl_env = e; cl_body = body } in
            eval (Closure closure :: s') t rest e  (* Push the closure onto the stack *) 
              | [] ->  eval [] ("Panic" :: t) [] e
                  (* FunError2: The stack is empty *)
              | _ -> 
                eval [] ("Panic" :: t) [] e (* FunError1: x is not a symbol *)
            )
      | Return :: rest -> (
              match s with
              | Closure { cl_name = _; cl_env = closure_env; cl_body = closure_body } :: return_val :: s' ->
                  let new_stack = return_val :: s' in  (* The new stack has the return value on top *)
                  eval new_stack t closure_body closure_env  (* Continue execution with the closure's body and environment *)
              | _ -> eval [] ("Panic" :: t) [] e (* Handle error cases such as an empty stack or a stack without a closure on top *)
            )
         

        (* ... rest of your code ... *)

| Call :: rest ->
  begin
    match s with
    | Closure(func) :: arg :: rest_of_stack -> (* Correct Call *)
      let new_env = (func.cl_name, Closure(func)) :: func.cl_env @ e in
      (* Execute the function's body with the new environment *)
      let result_trace = eval [arg] t func.cl_body new_env in
      (* Continue with the rest of the program, using the new trace and stack *)
      eval rest_of_stack result_trace rest e

    | Closure(_) :: [] | [] -> (* CALLERROR2 and CALLERROR3: Stack is empty or has only one element *)
      eval [] ("Panic" :: t) [] e

    | _ -> (* CALLERROR1: Top of stack is not a closure *)
      eval [] ("Panic" :: t) [] e
  end

(* ... rest of your code ... *)

         
            
     
              
         
            
let initial_env : env = []  (* This would contain any predefined bindings *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms()) s with
  | Some (p, []) -> 
    let initial_stack = [] (* Starting with an empty stack *) in
    let initial_trace = [] (* Starting with an empty trace *) in
    let final_trace = eval initial_stack initial_trace p initial_env in
    Some final_trace (* Return the final trace after evaluation *)
  | Some (_, _ :: _) -> 
    None (* Parsing did not consume the entire input, which indicates an error *)
  | None -> 
    None (* Parsing failed, likely due to a syntax error in the input program *)
      

