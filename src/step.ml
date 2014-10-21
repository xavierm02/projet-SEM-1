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
	    | None -> failwith ("Uninitialized_Variable " ^ (v |> string_of_var) ^ "!")
	 end
    | Expr_Plus (e1, e2) -> eval_binop (lift_binop_int_int (+)) e1 e2 sigma
    | Expr_Minus (e1, e2) -> eval_binop (lift_binop_int_int (-)) e1 e2 sigma
    | Expr_Mult (e1, e2) -> eval_binop (lift_binop_int_int ( * )) e1 e2 sigma
    | Expr_Div (e1, e2) -> eval_binop (lift_binop_int_int (/)) e1 e2 sigma
    | Expr_Not e1 -> eval_unop (lift_unop_bool (not)) e1 sigma
    | Expr_And (e1, e2) -> eval_binop (lift_binop_bool_bool (&&)) e1 e2 sigma
    | Expr_Or (e1, e2) -> eval_binop (lift_binop_bool_bool (||)) e1 e2 sigma
    | Expr_Equal (e1, e2) -> eval_binop (lift_binop_int_bool (=)) e1 e2 sigma
    | Expr_NotEqual (e1, e2) -> eval_binop (lift_binop_int_bool (<>)) e1 e2 sigma
    | Expr_Less (e1, e2) -> eval_binop (lift_binop_int_bool (<)) e1 e2 sigma
    | Expr_LessEqual (e1, e2) -> eval_binop (lift_binop_int_bool (<=)) e1 e2 sigma
    | Expr_Greater (e1, e2) -> eval_binop (lift_binop_int_bool (>)) e1 e2 sigma
    | Expr_GreaterEqual (e1, e2) -> eval_binop (lift_binop_int_bool (>=)) e1 e2 sigma
    | Expr_PostPlus v
    | Expr_PostMinus v ->
      let op =
        match expr with
        | Expr_PostPlus _ -> lift_unop_int ((+) 1)
        | Expr_PostMinus _ -> lift_unop_int (fun x -> x - 1)
        | _ -> failwith "Impossible!"
      in
      let (v1, sigma') = eval_expr (Expr_Var v) sigma in
      (op v1, sigma')
    | Expr_PrePlus v -> eval_expr (Expr_EAssign (v, (Expr_Plus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_PreMinus v -> eval_expr (Expr_EAssign (v, (Expr_Minus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_EAssign (v, e) ->
      let (v1, sigma') = eval_expr e sigma in
      (v1, (update_env v v1 sigma'))
    | Expr_String s -> (Utils.string_to_value s, sigma)
    | Expr_Parse e -> begin
      let (v1, sigma') = eval_expr e sigma in
      match v1 with
      | String s ->
        let p = ToyParser.make_prog ToyLexer.make_token (Lexing.from_string s) in
        (Prog p, sigma)
      | _ -> failwith "Parse can only be applied to String values."
    end
    | Expr_Prog p -> (Prog p, sigma)
    | Expr_Cons (e1, e2) ->
      eval_binop (fun v1 v2 -> String ((value_to_string v1) ^ (value_to_string v2))) e1 e2 sigma
    | Expr_Unsupported -> failwith "Unsupported expression!"

(** Type des configurations d'exécution *)
type outcome =
  | Continue of prog * ToyEnv.env
  | Finished of ToyEnv.env

let env_of_outcome = function
  | Continue (_, sigma)
  | Finished sigma -> sigma

let add_default_label x = ((Tau, None), x)

(** Relation de transition SOS de TOY. L'appel [step (p,s)] exécute un
    petit pas *)
let rec step (p, (sigma: ToyEnv.env)) : label * outcome =
  match p with
  | Skip -> Finished sigma |> add_default_label
  | Assign (v, e) ->
    let (v1, sigma') = (eval_expr e sigma) in
    Finished (update_env v v1 sigma') |> add_default_label
  | Seq (p1, p2) -> begin
    let label, outcome = step (p1, sigma) in
    let label_exception, label_print = label in
    match label_exception with
    | Tau -> begin
      match outcome with
      | Continue (p1', sigma') -> (label, Continue (Seq (p1', p2), sigma'))
      | Finished sigma' -> (label, Continue (p2, sigma'))
    end
    | Label _ -> begin
      (label, Finished (outcome |> env_of_outcome))
    end
  end
  | If (e, p1, p2) -> 
    let (v1, sigma' ) = (eval_expr e sigma) in
    if (value_to_bool v1) then
      Continue (p1, sigma') |> add_default_label
    else
      Continue (p2, sigma') |> add_default_label
  | While (e, p1) -> begin
    let (v1, sigma' ) = (eval_expr e sigma) in
    if (value_to_bool v1) then
      Continue (Seq (p1, p), sigma') |> add_default_label
    else
      Finished sigma' |> add_default_label
  end
  | For (var, e1, e2, p1) -> begin
    let (v1, sigma1 ) = (eval_expr e1 sigma) in
    let (v2, sigma2 ) = (eval_expr e2 sigma1) in
    let sigma3 = update_env var v1 sigma1 in
    if (value_to_int v1) < (value_to_int v2) then
      Continue (Seq (p1, For(var, Expr_PrePlus(var), Expr_Num (value_to_int v2), p1)), sigma3) |> add_default_label
    else if (value_to_int v1) > (value_to_int v2) then
      Continue (Seq (p1, For(var, Expr_PreMinus(var), Expr_Num (value_to_int v2), p1)), sigma3) |> add_default_label
    else
      Finished sigma3 |> add_default_label
  end 
  | Print expr -> begin
    let (v1, sigma' ) = (eval_expr expr sigma) in
    ((Tau, Some v1), Finished sigma')
  end
  | Try (p1, try_label, p2) -> begin
    let label, outcome = step (p1, sigma) in
    let label_exception, label_print = label in
    match label_exception with
    | Tau -> begin
      match outcome with
      | Continue (p1', sigma') -> (label, Continue (Try (p1', try_label, p2), sigma'))
      | Finished sigma' -> (label, Finished sigma')
    end
    | Label _ -> begin
      let sigma' = outcome |> env_of_outcome in
      if label_exception = try_label then
        ((Tau, label_print), Continue (p2, sigma'))
      else
	(label, Finished sigma')
    end
  end
  | Raise l -> ((Label l, None), Finished sigma)
  | Eval e -> begin
    let (v1, sigma') = (eval_expr e sigma) in
    match v1 with
    | Prog p -> begin
      let label, outcome = step (p, sigma') in
      let label_exception, label_print = label in
      match label_exception with
      | Tau -> begin
	match outcome with
	| Continue (p', sigma'') -> (label, Continue (Eval (Expr_Prog p'), sigma''))
	| Finished sigma'' -> (label, Finished sigma'')
      end
      | Label _ -> (label, Finished (outcome |> env_of_outcome))
    end
    | _ -> failwith "Eval can only be applied to Prog values!"
  end
  | Unsupported -> begin
    ToyPrinter.print_prog p;
    raise Unsupported_Command |> add_default_label
  end

(** Fermeture reflexive-transitive de [step] *)
let rec run (p, sigma) : ToyEnv.env =
  print_env sigma;
  let label, outcome = step (p, sigma) in
  "↓ " ^ (label |> string_of_label) ^ "\n" |> print_string;
  let label_exception, label_print = label in
  match label_exception with
  | Tau -> begin
    match outcome with
    | Continue (p', sigma') -> run (p', sigma')
    | Finished sigma' -> print_env sigma'; sigma'
  end
  | Label l -> begin
    let sigma' = outcome |> env_of_outcome in
    print_env sigma';
    "Exception: " ^ l ^ "\n" |> print_string;
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
