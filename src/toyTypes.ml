(** Types des AST et des valeurs pour le langage TOY *)

(** {2 Types des AST pour le langage TOY} *)

(** Type des variables TOY *)
type var = Var of string

(** Type des valeurs TOY *)
type value = 
  | Int of int
  | Bool of bool
  | String of string
  | Prog of prog

and label_exception = Tau | Label of string

and label_print = value option

(** Type des labels TOY *)
and label = label_exception * label_print

(** Type des expressions TOY *)
and expr =
  | Expr_Num of int
  | Expr_Var of var
  | Expr_Plus of (expr * expr)
  | Expr_Minus of (expr * expr)
  | Expr_Mult of (expr * expr)
  | Expr_Div of (expr * expr)
  | Expr_Not of expr
  | Expr_And of (expr * expr)
  | Expr_Or of (expr * expr)
  | Expr_Equal of (expr * expr)
  | Expr_NotEqual of (expr * expr)
  | Expr_Less of (expr * expr)
  | Expr_LessEqual of (expr * expr)
  | Expr_Greater of (expr * expr)
  | Expr_GreaterEqual of (expr * expr)
  | Expr_PostPlus of var
  | Expr_PostMinus of var
  | Expr_PrePlus of var
  | Expr_PreMinus of var
  | Expr_EAssign of (var * expr)
  | Expr_String of string
  | Expr_Parse of expr
  | Expr_Prog of prog
  | Expr_Unsupported

(** Type des programmes TOY *)
and prog =
  | Skip
  | Assign of (var * expr)
  | Seq of (prog * prog)
  | If of (expr * prog * prog)
  | While of (expr * prog)
  | For of (var * expr * expr * prog)
  | Print of (expr)
  | Try of (prog * label_exception * prog)
  | Raise of (string)
  | Eval of expr
  | Unsupported

(** {2 Valeurs sémantiques de TOY} *)

exception Unsupported_Expression
exception Uninitialized_Variable of var
exception Unsupported_Command

