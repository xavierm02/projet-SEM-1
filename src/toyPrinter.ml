open ToyTypes
open Printf

let string_of_var : var -> string =
  function
  | Var(s) -> s

let output_var (v: var) (oc: out_channel) : unit =
  fprintf oc "%s" (string_of_var v)

let string_of_value : value -> string =
  function
  | Int(i) -> string_of_int i

let output_value (v: value) (oc: out_channel) : unit =
  fprintf oc "%s" (string_of_value v)

let output_label (l: label) (oc: out_channel) : unit =
  match l with
  | Label(l) -> fprintf oc "%s" l
  | Tau -> fprintf oc "τ"

type priority =
  | Prio_MIN
  | Prio_Assign
  | Prio_Or
  | Prio_And
  | Prio_Not
  | Prio_Comp
  | Prio_Plus
  | Prio_Mult
  | Prio_Unary
  | Prio_MAX

(* TODO: associativity *)
let output_expr : expr -> out_channel -> unit =
  let paren chr ctxt prio oc =
    if ctxt >= prio then fprintf oc "%c" chr
  in
  let paren_open  = paren '(' in
  let paren_close = paren ')' in
  let print_binop f ctxt prio op l r oc =
      fprintf oc
      "%t%t%s%t%t"
      (paren_open ctxt prio)
      (f prio l)
      op
      (f prio r)
      (paren_close ctxt prio)
  in
  let rec aux ctxt = function
  | Expr_Var(s) -> output_var s
  | Expr_Num(i) -> output_value (Int i)
  | Expr_Plus(e,f) -> print_binop aux ctxt Prio_Plus " + " e f
  | Expr_Mult(e,f) -> print_binop aux ctxt Prio_Mult " × " e f
  | Expr_Equal(e,f) -> print_binop aux ctxt Prio_Comp " = " e f
  | Expr_Less(e,f) -> print_binop aux ctxt Prio_Comp " < " e f
  | Expr_Unsupported -> failwith "output_expr: Unsupported expression"
  in
  aux Prio_MIN

let print_expr (e: expr) : unit =
  output_expr e stdout

let output_prog : prog -> out_channel -> unit =
  let make_indent n : string = String.make (2 * n) ' ' in
  let indented ind oc str =
    fprintf oc "%s%s" (make_indent ind) str
  in
  let rec aux ind oc = function
  | Skip -> fprintf oc "%a" (indented ind) "skip"
  | Assign(v,e) ->
      fprintf oc "%s%t := %t"
      (make_indent ind)
      (output_var v)
      (output_expr e)
  | Seq(p,q) ->
      fprintf oc "%a;\n%a"
      (aux ind) p
      (aux ind) q
  | If(cond, if_so, if_not) ->
      fprintf oc "%a %t\n%a\n%a\n%a\n%a\n%a"
      (indented ind) "if"
      (output_expr cond)
      (indented ind) "then"
      (aux (ind + 1)) if_so
      (indented ind) "else"
      (aux (ind + 1)) if_not
      (indented ind) "end"
  | While(cond, body) ->
      fprintf oc "%a %t do\n%a\n%a"
      (indented ind) "while"
      (output_expr cond)
      (aux (ind + 1)) body
      (indented ind) "end"
  | Print(e) ->
      fprintf oc "%a %t"
      (indented ind) "print"
      (output_expr e)
  | Try(body, lbl, handler) ->
      fprintf oc "%a\n%a\n%a %t ->\n%a\n%a"
      (indented ind) "try"
      (aux (ind + 1)) body
      (indented ind) "with"
      (output_label lbl)
      (aux (ind + 1)) handler
      (indented ind) "end"
  | Raise(e) ->
      fprintf oc "%a %s"
      (indented ind) "raise"
      e
  | Unsupported -> failwith "output_prog: unsupported program"
  in
  fun p oc -> aux 0 oc p

let print_prog (p: prog) : unit =
  fprintf stdout "%t\n"
  (output_prog p)
