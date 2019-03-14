(* LPATO INTERPRETER *)
(* Patrick Ferris 2019 *)

exception Error of string 

open Types

let complain s = raise (Error s)

type var = string

type value = 
  | INT    of int 
  | CLOSURE of closure
  | LAST_ARG

and closure = code * env

and instruction = 
  | PUSH of value
  | LOOKUP of var
  | OPER of boper
  | SWAP
  | POP
  | BIND of var 
  | MK_CLOSURE of code
  | APPLY 

and code = instruction list 
and binding = var * value
and env = binding list

type env_or_value = EV of env | V of value

let mk_function (c, env) = CLOSURE(c, env)

let lookup_opt (env, x) = 
  let rec aux = function 
    | [] -> None
    | (y, v) :: rest ->
      if x = y then Some v else aux rest 
  in aux env

let rec search (evs, x) = 
  match evs with
  | [] -> complain (x ^ " is not defined \n")
  | (V _) :: rest -> search (rest, x)
  | (EV env) :: rest ->
    (match lookup_opt (env, x) with
       | None -> search (rest, x)
       | Some v -> v
    )
let string_of_list sep f l = 
  let rec aux f = function
  | []    -> ""
  | [t]   -> (f t)
  | (t :: rest) -> (f t) ^ sep ^ (aux f rest)
  in "[" ^ (aux f l) ^ "]"

let string_of_value = function 
  | INT n     -> string_of_int n
  | CLOSURE _ -> "Closure(...)"
  | LAST_ARG  -> "Last Argument..."

let string_of_instruction = function 
  | PUSH value -> "PUSH " ^ (string_of_value value)
  | LOOKUP x -> "LOOKUP " ^ x
  | OPER _ -> "OPERATION"
  | SWAP -> "SWAP"
  | POP  -> "POP"
  | BIND x -> "BIND " ^ x
  | MK_CLOSURE _ -> "CLOSURE"
  | APPLY -> "APPLY"


let string_of_code c = string_of_list ",\n " string_of_instruction c
let string_of_binding (x, v) = "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"
let string_of_env env = string_of_list ",\n " (string_of_binding) (env)

let string_of_env_or_value = function
  | EV env -> "EV " ^ (string_of_env env)
  | V v -> "V " ^ (string_of_value v)

let string_env = string_of_list ";\n " string_of_env_or_value 
let string_stack (c, evs) =
 "\n Code Stack: \n" ^ (string_of_code c)
 ^ "\n Env Stack: \n" ^ (string_env evs) 

let rec evs_to_env = function
  | [] -> []
  | (V _) :: rest -> evs_to_env rest
  | (EV env) :: rest -> env @ (evs_to_env rest)

let do_oper = function
  | (ADD,  INT n, INT m)  ->  INT (n + m)
  | (MULT, INT n, INT m)  ->  INT (n * m)
  | (SUB,  INT n, INT m)  ->  INT (n - m)
  | (DIV,  INT n, INT m)  ->  INT (n / m)
  | (_, _, _) -> complain ("WRONG OPER")

let leave_scope = [SWAP; POP]

let rec get_values env acc1 = match env with 
  | [] -> (acc1, [])
  | ((EV _) :: _) -> (acc1, [])
  | ((V LAST_ARG) :: rest) -> (acc1, rest)
  | ((V v) :: rest) -> get_values rest ((V v) :: acc1)
  
(* STEP FUNCTION - (Code Stack, Environment Stack) *)
let step = function 
  | ((PUSH v) :: codes, evs)                      -> (codes, (V v) :: evs) 
  | ((POP) :: codes, evs)                    -> (codes, List.tl evs)
  | ((SWAP) :: codes, e1 :: e2 :: evs)            -> (codes, e2 :: e1 :: evs)
  | ((BIND x) :: codes, (V v) :: evs)             -> (codes, EV([x, v]) :: evs)
  | ((BIND x) :: codes, (EV(env)) :: (V v) :: evs)  -> (codes, EV((x, v)::env)::evs)
  | ((LOOKUP x) :: codes, evs)                    -> (codes, V(search(evs,x)) :: evs)
  | ((OPER op) :: codes, (V v1) :: (V v2) :: evs) -> (codes, V(do_oper(op, v1, v2)) :: evs)
  | ((MK_CLOSURE c) :: codes, evs)                -> (codes, V(mk_function(c, evs_to_env evs)) :: evs)
  | ((APPLY :: codes), V(CLOSURE (c, env)) :: evs) -> let (values, rest) = get_values evs ([]) 
                                                      in (c @ codes,  values @ [EV env]  @ rest)
  | (c, env) -> complain (string_stack (c, env))


(* COMPILING THE TYPES.ML TO STACK INSTRUCTIONS *)
let rec compile = function 
  | Integer n               -> [PUSH (INT n)]
  | Var x                   -> [LOOKUP x]
  | Operator(e1, op, e2)    -> (compile e1) @ (compile e2) @ [OPER op]
  | Lambda (vars, e)        -> let bound_variables = bind_vars vars in 
                               [MK_CLOSURE(bound_variables @ (compile e) @ leave_scope)]
  | Func (f, (vars, e), e2) -> let bound_variables = bind_vars vars in 
                               (MK_CLOSURE(bound_variables @ (compile e) @ leave_scope)) ::
			       (BIND f) :: (compile e2) @ leave_scope
  | Application (e1, e2)    -> let compiled_arguments = compile_arg e2 in
                               (compiled_arguments) @ (compile e1) @ [APPLY; SWAP; POP]

and bind_vars = function
  | []     -> []
  | (x::xs) -> (bind_vars xs) @ [BIND x]

and compile_arg = function 
  | []      -> [PUSH LAST_ARG]
  | (e::es) -> (compile_arg es) @ (compile e)

(* PRINTING FUNCTIONS *)
let empty_env = []

let rec driver n state = 
  let _ = if false 
  then print_string ("N: " ^ (string_of_int n) ^ " : " ^ (string_stack state)) else ()
  in match state with
  | ([], [V v])  -> v
  | _ -> driver (n + 1) (step state)

let interpret_top e = 
  let c = compile e in driver 1 (c, empty_env) 

