(* Layout of implementation:
   - Lexer
   - Parser
   - Evaluator
   - Output *)

   let is_lower_case c = 'a' <= c && c <= 'z'
   let is_upper_case c = 'A' <= c && c <= 'Z'
   let is_alpha c = is_lower_case c || is_upper_case c
   let is_digit c = '0' <= c && c <= '9'
   let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c
   let is_blank c = String.contains " \012\n\r\t" c
   let explode s = List.of_seq (String.to_seq s)
   let implode ls = String.of_seq (List.to_seq ls)
   
   let andthen f opt =
     match opt with
     | Some res -> f res
     | None -> None
   
   type 'a stream_node =
     | Stream_nil
     | Stream_cons of 'a * 'a stream
   and 'a stream = unit -> 'a stream_node
   
   type token =
   | INT of int
   | BOOL of bool
   | UNIT
   | SYMBOL of string
   | PUSH
   | POP
   | ADD
   | SUB
   | MUL
   | DIV
   | AND
   | OR
   | NOT
   | LT
   | GT
   | IF
   | ELSE
   | END
   | BIND
   | LOOKUP
   | FUN
   | CALL
   | RETURN
   |SWAP
   |TRACE
 
   
   let reserved =
     [ 
      ("Push", PUSH)
     ; ("Pop", POP)
     ; ("Add", ADD)
     ; ("Sub", SUB)
     ; ("Mul", MUL)
     ; ("Div", DIV)
     ; ("Swap", SWAP)
     ; 

        ("And", AND)
     ; ("Or", OR)
     ; ("Not", NOT)
    
     ; ("IfThen", IF)
    
     ; ("Fun", FUN)
   
     ; ("Call", CALL)
     ; ("Return", RETURN)
     ]
   
   let rec trim_ws cs =
     match cs with
     | [] -> []
     | c :: cs ->
       if is_blank c then
         trim_ws cs
       else
         c :: cs
   
   let rec try_match cs fs =
     match fs with
     | f :: fs -> (
       match f cs with
       | Some (res, cs) -> Some (res, trim_ws cs)
       | None -> try_match cs fs)
     | [] -> None
   
   let match_nat cs =
     let rec loop cs =
       match cs with
       | c :: cs ->
         if '0' <= c && c <= '9' then
           let digits, cs = loop cs in
           (c :: digits, cs)
         else
           ([], c :: cs)
       | [] -> ([], [])
     in
     match cs with
     | c :: cs ->
       if '0' <= c && c <= '9' then
         let digits, cs = loop cs in
         let i = int_of_string (implode (c :: digits)) in
         Some (INT i, cs)
       else
         None
     | _ -> None
   
     let match_int cs =
      let rec gather_digits cs acc =
        match cs with
        | c :: cs' when is_digit c -> gather_digits cs' (c :: acc)
        | _ -> (List.rev acc, cs)
      in
      match cs with
      | '-' :: cs' -> 
        let (digits, cs'') = gather_digits cs' [] in
        if digits = [] then None else Some (INT (-int_of_string (implode digits)), cs'')
      | _ -> 
        let (digits, cs') = gather_digits cs [] in
        if digits = [] then None else Some (INT (int_of_string (implode digits)), cs')
        let match_symbol cs =
          let rec gather_symbol_chars cs acc =
            match cs with
            | c :: cs' when is_alphanum c || c = '_' || c = '\'' -> gather_symbol_chars cs' (c :: acc)
            | _ -> (List.rev acc, cs)
          in
          match cs with
          | c :: cs' when is_alpha c || c = '_' || c = '\'' -> 
            let (symbol_chars, cs'') = gather_symbol_chars cs' [c] in
            Some (SYMBOL (implode symbol_chars), cs'')
          | _ -> None
            
 
 
   
   let match_head (s, res) cs =
     let cs0 = explode s in
     let rec loop cs0 cs =
       match cs0, cs with
       | [], _ -> Some (res, cs)
       | c0 :: cs0, c :: cs ->
         if c0 = c then
           loop cs0 cs
         else
           None
       | _ -> None
     in
     loop cs0 cs
   
     let lexer s =
      let cs = explode s in
      let rec loop cs =
        fun () ->
          match cs with
          | [] -> Stream_nil
          | _ -> (
            let opt =
              try_match cs
                (match_int ::
                 match_symbol ::  (* Replacing match_name with match_symbol *)
                 (* match_string :: *) (* Remove or comment out if strings are not part of your language *)
                 List.map match_head reserved)
            in
            match opt with
            | Some (tok, cs) -> Stream_cons (tok, loop cs)
            | None -> Stream_nil)
      in
      fun () -> loop (trim_ws cs) ()
    
   
   let next_tok toks =
     match toks () with
     | Stream_cons (tok, toks) -> Some (tok, toks)
     | Stream_nil -> None
   
     type const =
     | Int of int
     | Bool of bool
     | Unit
     | Symbol of string
   
     type com =
     | Push of const
     | Pop
     | Add
     | Sub
     | Mul
     |Swap
     | Div
     | And
     | Or
     | Not
     | Lt
     | Gt
     | If of prog * prog (* If command followed by two programs (then and else branches) *)
     | Bind
     | Lookup
     | Fun of prog (* Function name and its program *)
     | Call
     | Return
     |Trace
   
   
     and prog = com list

     and decl = string * prog (* Function name and its program *)

   
     let rec parse_cmd toks =
      next_tok toks |> andthen @@ fun (tok, toks) ->
      match tok with
      | PUSH ->
        parse_const toks |> andthen @@ fun (const, toks) -> Some (Push const, toks)
      | POP -> Some (Pop, toks)
      | ADD -> Some (Add, toks)
      | SUB -> Some (Sub, toks)
      | MUL -> Some (Mul, toks)
      | DIV -> Some (Div, toks)
      | AND -> Some (And, toks)
      | OR -> Some (Or, toks)
      | NOT -> Some (Not, toks)
      | LT -> Some (Lt, toks)
      | GT -> Some (Gt, toks)
      | IF ->
        parse_cmds toks       |> andthen @@ fun (then_cmds, toks) ->
        parse_ender ELSE toks |> andthen @@ fun (_, toks) ->
        parse_cmds toks       |> andthen @@ fun (else_cmds, toks) ->
        parse_ender END toks  |> andthen @@ fun (_, toks) ->
        Some (If (then_cmds, else_cmds), toks)
      | FUN ->
        parse_cmds toks      |> andthen @@ fun (fun_body, toks) ->
        parse_ender END toks |> andthen @@ fun (_, toks) ->
        Some (Fun fun_body, toks)
      | CALL -> Some (Call, toks)
      | RETURN -> Some (Return, toks)
      | BIND -> Some (Bind, toks)
      | LOOKUP -> Some (Lookup, toks)
      | TRACE -> Some (Trace, toks)  (* Corrected *)
      | _ -> None  (* Additional tokens need to be handled here *)
    
    and parse_const toks =
      next_tok toks |> andthen @@ fun (tok, toks) ->
      match tok with
      | INT i -> Some (Int i, toks)
      | BOOL b -> Some (Bool b, toks)
      | UNIT -> Some (Unit, toks)
      | SYMBOL s -> Some (Symbol s, toks)
      | _ -> None
    
    and parse_cmds toks =
      let rec aux cmds toks =
        match parse_cmd toks with
        | Some (cmd, toks') -> aux (cmd :: cmds) toks'
        | None -> (List.rev cmds, toks)  (* This needs to be wrapped in Some *)
      in
      let (cmds, toks') = aux [] toks in
      Some (cmds, toks')  (* Return the parsed commands wrapped in Some *)
    
    and parse_ender keyword toks =
      next_tok toks |> andthen @@ fun (tok, toks) ->
      if keyword = tok then
        Some ((), toks)
      else
        None
    
   
        type value =
        | VInt of int
        | VBool of bool
        | VUnit
        | VSymbol of string
        | VClo of env * prog  (* Closure contains environment and program *)
      
      and env = (string * value) list
     
      type stack = value list
type trace = string list

type result =
  | Ok of stack * trace
  | Error of string
      let string_of_bool b = if b then "true" else "false"

      let rec string_of_value v =
        match v with
        | VInt i -> string_of_int i
        | VBool b -> string_of_bool b
        | VUnit -> "Unit"
        | VSymbol s -> s
        | VClo (env, prog) -> "Closure(" ^ string_of_env env ^ ", " ^ string_of_prog prog ^ ")"
      
      and string_of_env env =
        "[" ^ (String.concat ", " (List.map (fun (name, value) -> name ^ " = " ^ string_of_value value) env)) ^ "]"
      
      and string_of_prog prog =
        "[" ^ (String.concat "; " (List.map string_of_com prog)) ^ "]"
      
      and string_of_com com = 
        match com with
        | Push c -> "Push " ^ (string_of_const c)
        | Pop -> "Pop"
        | Swap -> "Swap"
        | Trace -> "Trace"
        | Add -> "Add"
        | Sub -> "Sub"
        | Mul -> "Mul"
        | Div -> "Div"
        | And -> "And"
        | Or -> "Or"
        | Not -> "Not"
        | Lt -> "Lt"
        | Gt -> "Gt"
        | If (prog1, prog2) -> "If (" ^ (string_of_prog prog1) ^ ", " ^ (string_of_prog prog2) ^ ")"
        | Fun prog -> "Fun (" ^ (string_of_prog prog) ^ ")"
        | Call -> "Call"
        | Return -> "Return"
        | Bind -> "Bind"
        | Lookup -> "Lookup"
        | _ -> "UnknownCommand"
      
      and string_of_const c = 
        match c with
        | Int i -> string_of_int i
        | Bool b -> string_of_bool b
        | Unit -> "Unit"
        | Symbol s -> s
      
    
 
   
   let rec string_of_stack st =
     match st with
     | [] -> ""
     | [ cst ] -> string_of_value cst
     | cst :: st -> string_of_value cst ^ "\n" ^ string_of_stack st
   
     let value_of_const cst =
      match cst with
      | Int i -> Some (Int i)
      | Bool b -> Some (Bool b)
      | Unit -> Some (Unit)
      | Symbol s -> Some (Symbol s)
   
   let bool_of_int i =
     match i with
     | 0 -> Some false
     | 1 -> Some true
     | _ -> None
   
   let int_of_bool b =
     if b then
       1
     else
       0
   
       let rec takei i ls acc =
        if i <= 0 then
          Some (acc, ls)
        else
          match ls with
          | v :: ls -> takei (i - 1) ls (v :: acc)
          | [] -> None
   
          let mk_clo env prog = VClo (env, prog)



    
       
       (* ... Previous definitions ... *)

let rec eval (env: env) (s: stack) (t: trace) (p: prog): result =
  match p with
  | [] -> Ok (s, t)
  | Push c :: p0 -> 
    let value = const_to_value c in  (* Convert const to value *)
    eval env (value :: s) t p0
  | Pop :: p0 -> (
      match s with
      | _ :: s0 -> eval env s0 t p0
      | [] -> Error "Panic: Stack underflow on Pop")
  | Trace :: p0 -> (
      match s with
      | c :: s0 -> eval env (VUnit :: s0) (string_of_value c :: t) p0
      | [] -> Error "Panic: Stack underflow on Trace")
  (* Implement cases for Add, Sub, Mul, Div, And, Or, Not, Lt, Gt *)
  | Add :: p0 -> (
      match s with
      | VInt a :: VInt b :: s0 -> eval env (VInt (a + b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Add")
  | Sub :: p0 -> (
      match s with
      | VInt a :: VInt b :: s0 -> eval env (VInt (a - b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Sub")
  | Mul :: p0 -> (
      match s with
      | VInt a :: VInt b :: s0 -> eval env (VInt (a * b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Mul")
  | Div :: p0 -> (
      match s with
      | VInt a :: VInt b :: s0 when b != 0 -> eval env (VInt (a / b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Div or division by zero")
  | And :: p0 -> (
      match s with
      | VBool a :: VBool b :: s0 -> eval env (VBool (a && b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for And")
  | Or :: p0 -> (
      match s with
      | VBool a :: VBool b :: s0 -> eval env (VBool (a || b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Or")
  | Not :: p0 -> (
      match s with
      | VBool a :: s0 -> eval env (VBool (not a) :: s0) t p0
      | _ -> Error "Panic: Invalid operand for Not")
  | Lt :: p0 -> (
      match s with
      | VInt a :: VInt b :: s0 -> eval env (VBool (a < b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Lt")
  | Gt :: p0 -> (
      match s with
      | VInt a :: VInt b :: s0 -> eval env (VBool (a > b) :: s0) t p0
      | _ -> Error "Panic: Invalid operands for Gt")
  (* Implement cases for Swap, Bind, Lookup, Fun, Call, Return, IfElse *)
  | Swap :: p0 -> (
      match s with
      | a :: b :: s0 -> eval env (b :: a :: s0) t p0
      | _ -> Error "Panic: Not enough elements for Swap")
  | Bind :: p0 -> (
      match s with
      | VSymbol sym :: v :: s0 -> eval ((sym, v) :: env) s0 t p0
      | _ -> Error "Panic: Invalid operands for Bind")
  | Lookup :: p0 -> (
      match s with
      | VSymbol sym :: s0 -> (
          match List.assoc_opt sym env with
          | Some v -> eval env (v :: s0) t p0
          | None -> Error ("Panic: Symbol " ^ sym ^ " not found in Lookup"))
      | _ -> Error "Panic: Invalid operand for Lookup")
  | Fun prog :: p0 -> eval env (VClo(env, prog) :: s) t p0
  | Call :: p0 -> (
      match s with
      | VClo(closure_env, closure_prog) :: v :: s0 ->
          eval closure_env (v :: []) t closure_prog  (* Adjust according to your language's call semantics *)
      | _ -> Error "Panic: Call expected a closure and an argument on the stack")
  | Return :: p0 -> (
      match s with
      | v :: s0 -> eval env s0 t p0  (* Adjust according to your language's return semantics *)
      | [] -> Error "Panic: Return command with an empty stack")
  | If(true_cmds, false_cmds) :: p0 -> (
      match s with
      | VBool b :: s0 -> 
          let branch_cmds = if b then true_cmds else false_cmds in
          eval env s0 t branch_cmds
      | _ -> Error "Panic: Invalid operand for IfElse")
  | _ -> Error "Panic: Unrecognized command"


and const_to_value c = 
  match c with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Unit -> VUnit
  | Symbol s -> VSymbol s
  


  let interp program =
    let initial_env = []  (* Initial environment *)
    and initial_stack = []  (* Initial stack *)
    and initial_trace = []  (* Initial trace *)
    in
    eval initial_env initial_stack initial_trace program