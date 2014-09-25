(** Définition de l'interpréteur TOY *)
open ToyTypes
open ToyEnv
open Utils

(** [eval_expr e s] évalue l'expression [e] dans l'environnement [s] *)
let rec eval_expr expr (sigma: ToyEnv.env) : value =
  match expr with
    | Expr_Num(n) -> Utils.int_to_value n
    | Expr_Var v -> begin
	    match eval_env v sigma with
	      | Some x -> x
	      | None -> raise (Uninitialized_Variable v)
	end
    | Expr_Plus (x, y) -> lift_binop (+) (eval_expr x sigma) (eval_expr y sigma)
    | Expr_Mult (x, y) -> lift_binop ( * ) (eval_expr x sigma) (eval_expr y sigma)
    | Expr_Equal (x, y) -> lift_binop_bool (=) (eval_expr x sigma) (eval_expr y sigma)
    | Expr_Less (x, y) -> lift_binop_bool (<) (eval_expr x sigma) (eval_expr y sigma)
    | Expr_Unsupported -> raise Unsupported_Expression

(** Type des configurations d'exécution *)
type outcome =
  | Continue of prog * ToyEnv.env
  | Finished of ToyEnv.env

(** Relation de transition SOS de TOY. L'appel [step (p,s)] exécute un
    petit pas *)
let rec step (p, (sigma: ToyEnv.env)) : outcome =
  match p with
  | Unsupported -> failwith "step: unsupported program"
  | Skip -> Finished sigma
  | _ ->
    prerr_endline "TODO step";
    Finished sigma

(** Fermeture reflexive-transitive de [step] *)
let rec run (p, sigma) : ToyEnv.env =
  match step (p, sigma) with
  | Continue (p', sigma') -> run (p', sigma')
  | Finished sigma' -> sigma'

(** [go_step filename] interpréte le programme TOY représenté en
    syntaxe concrète dans le fichier de nom [filename]. *)
let go_step file =
  let print_line () = print_endline "----------" in
  let prog = Utils.parse file in
  print_endline "Structured Operational Semantics, small steps";
  print_line ();
  ToyPrinter.print_prog prog;
  print_line ();
  let _ = run (prog, ToyEnv.init_env) in
  print_line ();
  print_endline "Program stopped!"