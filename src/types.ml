type boper = ADD | SUB | MULT | DIV;;

type expr = 
  | Integer of int
  | Operator of expr * boper * expr 
  | Lambda of lambda
  | Func of var * lambda * expr
  | Application of expr * (expr list)
  | Var of var

  and var = string
  and lambda = (var list) * expr

open Format

let pp_bop op = 
  match op with 
  | ADD  -> "+"
  | SUB  -> "-"
  | MULT -> "*"
  | DIV  -> "/"

let fstring ppf s = fprintf ppf "%s" s
let pp_binary ppf op = fstring ppf (pp_bop op)

let rec pp_expr ppf = function
  | Integer i                     -> fstring ppf (string_of_int i)
  | Operator (e1, op, e2)         -> fprintf ppf "(%a %a %a)" pp_expr e1 pp_binary op pp_expr e2
  | Lambda (_, expr)            -> fprintf ppf "(~lambda %a => %a)" fstring "tuple"  pp_expr expr
  | Func (func, (_, e1), e2)    -> fprintf ppf "(FUNC:%a(%a) => %a in %a)" fstring func fstring "tuple"  pp_expr e1 pp_expr e2
  | Application (e1, _)        -> fprintf ppf "(Applying %a)" pp_expr e1
  | Var (var) -> fstring ppf var

let print_expr e = 
  let _ = pp_expr std_formatter e
  in print_flush ()
