(** Définition de l'interpréteur TOY *)
open ToyTypes
open ToyEnv
open Utils
open ToyPrinter

(** [eval_expr e s] évalue l'expression [e] dans l'environnement [s] *)
let rec eval_expr expr (sigma: ToyEnv.env) : value * env =
  let eval_unop op e1 sigma =
    let (v1, sigma') = eval_expr e1 sigma in
    (op v1, sigma')
  in
  let eval_binop op e1 e2 sigma =
     let (v1, sigma') = eval_expr e1 sigma in
     let (v2, sigma'') = eval_expr e2 sigma' in
     (op v1 v2, sigma'')
  in
  match expr with
    | Expr_Num n -> (Utils.int_to_value n, sigma)
    | Expr_Var v -> begin
	  match eval_env v sigma with
	    | Some x -> (x, sigma)
	    | None -> failwith ("Uninitialized_Variable " ^ (v |> string_of_var))
	 end
    | Expr_Plus (e1, e2) -> eval_binop (lift_binop (+)) e1 e2 sigma
    | Expr_Minus (e1, e2) -> eval_binop (lift_binop (-)) e1 e2 sigma
    | Expr_Mult (e1, e2) -> eval_binop (lift_binop ( * )) e1 e2 sigma
    | Expr_Div (e1, e2) -> eval_binop (lift_binop (/)) e1 e2 sigma
    | Expr_Not e1 -> eval_unop (lift_unop_bool not) e1 sigma
    | Expr_And (e1, e2) -> eval_binop (lift_binop_bool_bool (&&)) e1 e2 sigma
    | Expr_Or (e1, e2) -> eval_binop (lift_binop_bool_bool (||)) e1 e2 sigma
    | Expr_Equal (e1, e2) -> eval_binop (lift_binop_bool_int (=)) e1 e2 sigma
    | Expr_NotEqual (e1, e2) -> eval_binop (lift_binop_bool_int (<>)) e1 e2 sigma
    | Expr_Less (e1, e2) -> eval_binop (lift_binop_bool_int (<)) e1 e2 sigma
    | Expr_LessEqual (e1, e2) -> eval_binop (lift_binop_bool_int (<=)) e1 e2 sigma
    | Expr_Greater (e1, e2) -> eval_binop (lift_binop_bool_int (>)) e1 e2 sigma
    | Expr_GreaterEqual (e1, e2) -> eval_binop (lift_binop_bool_int (>=)) e1 e2 sigma
    | Expr_PostPlus v
    | Expr_PostMinus v ->
      let op =
        match expr with
        | Expr_PostPlus _ -> lift_unop ((+) 1)
        | Expr_PostMinus _ -> lift_unop (fun x -> x - 1)
        | _ -> failwith "Impossible!"
      in
      let (v1, sigma') = eval_expr (Expr_Var v) sigma in
      (op v1, sigma')
    | Expr_PrePlus v -> eval_expr (Expr_EAssign (v, (Expr_Plus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_PreMinus v -> eval_expr (Expr_EAssign (v, (Expr_Minus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_EAssign (v, e) ->
	  let (v1, sigma') = eval_expr e sigma in
	  (v1, (update_env v v1 sigma'))
	| Expr_Unsupported -> failwith "Unsupported expression."

(** Type des configurations d'exécution *)
type outcome =
  | Continue of prog * ToyEnv.env
  | Finished of ToyEnv.env

let env_of_outcome = function
  | Continue (_, sigma)
  | Finished sigma -> sigma

let add_tau x = (Tau, x)

let print_label = function
      | Label l -> print_string l; print_newline ()
      | _ -> failwith "plop"
      

(** Relation de transition SOS de TOY. L'appel [step (p,s)] exécute un
    petit pas *)
let rec step (p, (sigma: ToyEnv.env)) : label * outcome =
  match p with
  | Skip -> Finished sigma |> add_tau
  | Assign (v, e) ->
    let (v1, sigma') = (eval_expr e sigma) in
    Finished (update_env v v1 sigma') |> add_tau
  | Seq (p1, p2) -> begin
    let label, outcome = step (p1, sigma) in
    match label with
    | Tau -> begin
      match outcome with
      | Continue (p1', sigma') -> Continue (Seq (p1', p2), sigma') |> add_tau
      | Finished sigma' -> Continue (p2, sigma') |> add_tau
    end
    | Label _ -> begin
      (label, Finished (outcome |> env_of_outcome))
    end
  end
  | If (e, p1, p2) -> 
    let (v1, sigma' ) = (eval_expr e sigma) in
    if (value_to_bool v1) then
      Continue (p1, sigma') |> add_tau
    else
      Continue (p2, sigma') |> add_tau
  | While (e, p1) -> begin
    let (v1, sigma' ) = (eval_expr e sigma) in
    if (value_to_bool v1) then
      Continue (Seq (p1, p), sigma') |> add_tau
    else
      Finished sigma' |> add_tau
  end
  | For (var, e1, e2, p1) -> begin
    let (v1, sigma1 ) = (eval_expr e1 sigma) in
    let (v2, sigma2 ) = (eval_expr e2 sigma1) in
    let sigma3 = update_env var v1 sigma1 in
    if (value_to_int v1) < (value_to_int v2) then
      Continue (Seq (p1, For(var, Expr_PrePlus(var), Expr_Num (value_to_int v2), p1)), sigma3) |> add_tau
    else if (value_to_int v1) > (value_to_int v2) then
      Continue (Seq (p1, For(var, Expr_PreMinus(var), Expr_Num (value_to_int v2), p1)), sigma3) |> add_tau
    else
      Finished sigma3 |> add_tau
  end 
  | Print expr -> begin
    print_string "> ";
    let (v1, sigma' ) = (eval_expr expr sigma) in
    print_value v1;
    print_newline ();
    Finished sigma |> add_tau
  end
  | Try (p1, try_label, p2) -> begin
    let label, outcome = step (p1, sigma) in
    match label with
    | Tau -> begin
      match outcome with
      | Continue (p1', sigma') -> Continue (Try (p1', try_label, p2), sigma') |> add_tau
      | Finished sigma' -> Finished sigma' |> add_tau
    end
    | Label _ -> begin
      let sigma' = outcome |> env_of_outcome in
      if label = try_label then
        Continue (p2, sigma') |> add_tau
      else
	(label, Finished sigma')
    end
  end
  | Raise l -> (Label l ,Finished sigma)
  | Unsupported -> begin
    ToyPrinter.print_prog p;
    raise Unsupported_Command |> add_tau
  end

(** Fermeture reflexive-transitive de [step] *)
let rec run (p, sigma) : ToyEnv.env =
  print_env sigma;
  let label, outcome = step (p, sigma) in
  match label with
  | Tau -> begin
    match outcome with
    | Continue (p', sigma') -> run (p', sigma')
    | Finished sigma' -> print_env sigma'; sigma'
  end
  | Label l -> begin
    "Exception: " ^ l ^ "\n" |> print_string;
    let sigma' = outcome |> env_of_outcome in
    print_env sigma';
    sigma'
  end

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
  print_endline "Program stopped."
