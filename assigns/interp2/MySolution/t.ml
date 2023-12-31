

type closure = {
  cl_name: string;          (* Function name *)
  cl_env: (string * const) list;  (* Closure environment *)
  cl_body: prog;            (* Closure body *)
}

and const =
  | Int of int
  | Bool of bool
  | Unit 
  | Sym of string
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

  let parse_unit =
  keyword "Unit" >> pure Unit



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




let rec eval (s : stack) (t : trace) (p : prog) (e:env) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t p0 e


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

    | Bind :: p0 -> 
      (match s with
        | Sym x :: v :: s0 -> eval s0 t p0 ((x, v) :: e)  
        | Sym x :: [] -> eval [] ("Panic" :: t) [] e    
        | [] -> eval [] ("Panic" :: t) [] e              
        | _ -> eval [] ("Panic" :: t) [] e)  

    | Lookup :: p0 ->
      (match s with
        | Sym x :: s0 -> 
             (match List.assoc_opt x e with
              | Some v -> eval (v :: s0) t p0 e  
              | None -> eval [] ("Panic" :: t) [] e)  
           | [] -> eval [] ("Panic" :: t) [] e  
           | _ -> eval [] ("Panic" :: t) [] e)  

    |IfElse (if_coms, else_coms) :: rest ->
            (match s with
             | Bool b :: s' -> 
                if b then eval s' t (if_coms @ rest) e 
                else eval s' t (else_coms @ rest) e 
            
             | _ -> eval [] ("Panic" :: t) [] e) 

      | Fun (body) :: rest -> (
        match s with
        | Sym x :: s' ->
            let closure = { cl_name = x; cl_env = e; cl_body = body } in
            eval (Closure closure :: s') t rest e  
            
                 
              | _ -> 
                eval [] ("Panic" :: t) [] e 
            )
      | Return :: rest -> (
              match s with
              | Closure { cl_name = _; cl_env = closure_env; cl_body = closure_body } :: return_val :: s' ->
                  let new_stack = return_val :: s' in  
                  eval new_stack t closure_body closure_env  
              | _ -> eval [] ("Panic" :: t) [] e)
            | Call :: rest -> (
              match s with
              | Closure { cl_name = f_name; cl_env = closure_env; cl_body = closure_body } :: arg :: s' ->
               
                let new_env = (f_name, Closure { cl_name = f_name; cl_env = closure_env; cl_body = closure_body }) :: closure_env in
                eval (arg :: s') t closure_body new_env
              | [] | [_] -> 
                eval [] ("Panic" :: t) [] e 
              | _ -> 
                eval [] ("Panic" :: t) [] e
            )
            
            
let initial_env : env = []  (* This would contain any predefined bindings *)

let interp (s : string) : string list option = (*your code*)