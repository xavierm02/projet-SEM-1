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
  | Skip -> Finished sigma
  | Assign (v, e) -> Finished (update_env v (eval_expr e sigma) sigma)
  | Seq (p1, p2) -> begin
    match step (p1, sigma) with
      | Continue (p1', sigma') -> Continue (Seq (p1', p2), sigma')
      | Finished sigma' -> Continue (p2, sigma')
  end
  | If (e, p1, p2) ->
    if (eval_expr e sigma |> value_to_int) <> 0 then
      step (p1, sigma)
    else
	  step (p2, sigma)
  | While (e, pb) -> begin
    if (eval_expr e sigma |> value_to_int) <> 0 then
      step (Seq (pb, p), sigma)
    else
	  Finished sigma
  end
  | Print expr -> begin
    print_string "> ";
    eval_expr expr sigma |> print_value;
    print_newline ();
    Finished sigma
  end
  | Try _
  | Raise _
  | Unsupported -> begin
    ToyPrinter.print_prog p;
    raise Unsupported_Command
  end

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
