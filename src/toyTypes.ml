(** Types des AST et des valeurs pour le langage TOY *)

(** {2 Types des AST pour le langage TOY} *)

(** Type des variables TOY *)
type var = Var of string

(** Type des labels TOY *)
type label = Tau | Label of string

(** Type des expressions TOY *)
type expr =
  | Expr_Num of int
  | Expr_Var of var
  | Expr_Plus of (expr * expr)
  | Expr_Minus of (expr * expr)
  | Expr_Mult of (expr * expr)
  | Expr_Div of (expr * expr)
  | Expr_PostPlus of var
  | Expr_PostMinus of var
  | Expr_PrePlus of var
  | Expr_PreMinus of var
  | Expr_EAssign of (var * expr)
  | Expr_Unsupported

type bexpr =
  (*| Expr_Num of int*)
  | BExpr_Var of var
  | BExpr_Equal of (expr * expr)
  | BExpr_And of (bexpr * bexpr)
  | BExpr_Less of (expr * expr)
  | BExpr_EAssign of (var * bexpr)
  | BExpr_Unsupported

(** Type des programmes TOY *)
type prog =
  | Skip
  | Assign of (var * expr)
  | Seq of (prog * prog)
  | If of (bexpr * prog * prog)
  | While of (bexpr * prog)
  | Print of (expr)
  | Try of (prog * label * prog)
  | Raise of (string)
  | Unsupported

(** {2 Valeurs sémantiques de TOY} *)

(** Type des valeurs TOY *)
type value = 
  | Int of int
  | Bool of bool

exception Unsupported_Expression
exception Uninitialized_Variable of var
exception Unsupported_Command
