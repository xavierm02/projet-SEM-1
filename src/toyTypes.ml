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
  | Expr_Mult of (expr * expr)
  | Expr_Equal of (expr * expr)
  | Expr_Less of (expr * expr)
  | Expr_Unsupported

(** Type des programmes TOY *)
type prog =
  | Skip
  | Assign of (var * expr)
  | Seq of (prog * prog)
  | If of (expr * prog * prog)
  | While of (expr * prog)
  | Print of (expr)
  | Try of (prog * label * prog)
  | Raise of (string)
  | Unsupported

(** {2 Valeurs sémantiques de TOY} *)

(** Type des valeurs TOY *)
type value = Int of int 

exception Unsupported_Expression
