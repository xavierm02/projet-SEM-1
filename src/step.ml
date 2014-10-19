(** Définition de l'interpréteur TOY *)
open ToyTypes
open ToyEnv
open Utils
open ToyPrinter

(** [eval_expr e s] évalue l'expression [e] dans l'environnement [s] *)
let rec eval_expr expr (sigma: ToyEnv.env) : int * env =
  match expr with
    | Expr_Num n -> (n, sigma)
    | Expr_Var v -> begin
	  match eval_env v sigma with
	    | Some x -> (x |> value_to_int, sigma)
	    | None -> failwith ("Uninitialized_Variable " ^ (v |> string_of_var))
	 end
    | Expr_Plus (e1, e2)
	| Expr_Minus (e1, e2)
	| Expr_Mult (e1, e2)
	| Expr_Div (e1, e2) -> begin
      let op =
        match expr with
        | Expr_Plus _ -> (+)
        | Expr_Minus _ -> (-)
        | Expr_Mult _ -> ( * )
        | Expr_Div _ -> (/)
        | _ -> failwith "Impossible!"
      in
      let (v1, sigma') = eval_expr e1 sigma in
      let (v2, sigma'') = eval_expr e2 sigma' in
      (op v1 v2, sigma'')
    end
    | Expr_PostPlus v
    | Expr_PostMinus v ->
      let op =
        match expr with
        | Expr_PostPlus _ -> ((+) 1)
        | Expr_PostMinus _ -> (fun x -> x - 1)
        | _ -> failwith "Impossible!"
      in
      let (v1, sigma') = eval_expr (Expr_Var v) sigma in
      (op v1, sigma')
    | Expr_PrePlus v -> eval_expr (Expr_EAssign (v, (Expr_Plus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_PreMinus v -> eval_expr (Expr_EAssign (v, (Expr_Minus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_EAssign (v, e) ->
	  let (v1, sigma') = eval_expr e sigma in
	  (v1, (update_env v (Int v1) sigma'))
    | _ -> raise Unsupported_Expression

let rec eval_bexpr bexpr (sigma: ToyEnv.env) : bool * env =
  match bexpr with
    | BExpr_Equal (e1, e2)
	| BExpr_Less (e1, e2) -> begin
	  let op =
	    match bexpr with
	    | BExpr_Equal _ -> (=)
        | BExpr_Less _ -> (<)
      in
      let (v1, sigma') = eval_expr e1 sigma in
      let (v2, sigma'') = eval_expr e2 sigma' in
      (op v1 v2, sigma'')
    end
    | BExpr_And (e1, e2) -> begin
      let op =
	    match bexpr with
        | BExpr_And _ -> (<)
      in
      let (v1, sigma') = eval_bexpr e1 sigma in
      let (v2, sigma'') = eval_bexpr e2 sigma' in
      (op v1 v2, sigma'')
    end
    | _ -> raise Unsupported_Expression

(** Type des configurations d'exécution *)
type outcome =
  | Continue of prog * ToyEnv.env
  | Finished of ToyEnv.env

(** Relation de transition SOS de TOY. L'appel [step (p,s)] exécute un
    petit pas *)
let rec step (p, (sigma: ToyEnv.env)) : outcome =
  match p with
  | Skip -> Finished sigma
  | Assign (v, e) -> let (v1, sigma') = (eval_expr e sigma) in
                     Finished (update_env v (Int v1) sigma')
  | Seq (p1, p2) -> begin
    match step (p1, sigma) with
      | Continue (p1', sigma') -> Continue (Seq (p1', p2), sigma')
      | Finished sigma' -> Continue (p2, sigma')
  end
  | If (e, p1, p2) -> 
    let (v1, sigma' ) = (eval_bexpr e sigma) in
    if v1 then
      Continue (p1, sigma')
    else
	  Continue (p2, sigma')
  | While (e, p1) -> begin
    let (v1, sigma' ) = (eval_bexpr e sigma) in
    if v1 then
      Continue (Seq (p1, p), sigma')
    else
	  Finished sigma'
  end
  | Print expr -> begin
    print_string "> ";
    let (v1, sigma' ) = (eval_expr expr sigma) in
    print_value (Int v1);
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
  print_env sigma;
  match step (p, sigma) with
  | Continue (p', sigma') -> run (p', sigma')
  | Finished sigma' -> print_env sigma'; sigma'

(** [go_step filename] interpréte le programme TOY représenté en
    syntaxe concrète dans le fichier de nom [filename]. *)
let go_step file =
  let print_line () = print_endline "----------" in
  Parsing.set_trace true;
  let prog = Utils.parse file in
  print_endline "Structured Operational Semantics, small steps";
  print_line ();
  ToyPrinter.print_prog prog;
  print_line ();
  let _ = run (prog, ToyEnv.init_env) in
  print_line ();
  print_endline "Program stopped."
