(** Définition de l'interpréteur TOY *)
open ToyTypes
open ToyEnv
open Utils
open ToyPrinter

(** [eval_expr e s] évalue l'expression [e] dans l'environnement [s] *)
let rec eval_expr expr (sigma: ToyEnv.env) : value_or_label * env =
  let eval_unop op e1 sigma =
    match eval_expr e1 sigma with
    | Value2 v1, sigma' -> (Value2 (op v1), sigma')
    | r -> r
  in
  let eval_binop op e1 e2 sigma =
    match eval_expr e1 sigma with
    | Value2 v1, sigma' -> begin
      match eval_expr e2 sigma' with
      | Value2 v2, sigma'' -> (Value2 (op v1 v2), sigma'')
      | r -> r
    end
    | r -> r
  in
  match expr with
    | Expr_Num n -> (Value2 (Utils.int_to_value n), sigma)
    | Expr_Var v -> begin
	  match eval_env v sigma with
	    | Some x -> (Value2 x, sigma)
	    | None -> (Label2 (Label "uninitialized_variable", Some (String ("Uninitialized_Variable " ^ (v |> string_of_var) ^ "!"))), sigma)
	 end
    | Expr_Plus (e1, e2) -> eval_binop (lift_binop_int_int (+)) e1 e2 sigma
    | Expr_Minus (e1, e2) -> eval_binop (lift_binop_int_int (-)) e1 e2 sigma
    | Expr_Mult (e1, e2) -> eval_binop (lift_binop_int_int ( * )) e1 e2 sigma
    | Expr_Div (e1, e2) -> begin
      try
        eval_binop (lift_binop_int_int (/)) e1 e2 sigma
      with
      | Division_by_zero -> (Label2 (Label "divide_by_zero", Some (String ("Divided by zero!"))), sigma)
    end
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
    | Expr_PostMinus v -> begin
      let op =
        match expr with
        | Expr_PostPlus _ -> lift_unop_int ((+) 1)
        | Expr_PostMinus _ -> lift_unop_int (fun x -> x - 1)
        | _ -> failwith "Impossible!"
      in
      let (x1, sigma') = eval_unop (fun x -> x) (Expr_Var v) sigma in
      match x1 with
      | Label2 l -> (Label2 l, sigma')
      | Value2 v1 -> (Value2 v1, (update_env v (op v1) sigma'))
    end
    | Expr_PrePlus v -> eval_expr (Expr_EAssign (v, (Expr_Plus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_PreMinus v -> eval_expr (Expr_EAssign (v, (Expr_Minus (Expr_Var v, Expr_Num 1)))) sigma
    | Expr_EAssign (v, e1) -> begin
      match eval_expr e1 sigma with
      | Value2 v1, sigma' -> (Value2 v1, (update_env v v1 sigma'))
      | r -> r
    end
    | Expr_String s -> (Value2 (Utils.string_to_value (s)), sigma)
    | Expr_Parse e -> begin
      let (v1, sigma') = eval_expr e sigma in
      match v1 with
      | Label2 _ -> (v1, sigma')
      | Value2 (String s) -> begin
        try
	  let p = ToyParser.make_prog ToyLexer.make_token (Lexing.from_string s) in
	  (Value2 (Prog p), sigma)
	with
	| Parsing.Parse_error -> (Label2 (Label "parse_error", Some (String ("Could not parse expression!"))), sigma)
      end
      | _ -> (Label2 (Label "parse_non_string", Some (String ("Parse can only be applied to String values!"))), sigma)
    end
    | Expr_Prog p -> (Value2 (Prog p), sigma)
    | Expr_Cons (e1, e2) ->
      eval_binop (fun v1 v2 -> String ((value_to_string v1) ^ (value_to_string v2))) e1 e2 sigma
    | Expr_Escape e1 ->
      eval_unop (fun v1 -> String (String.escaped (value_to_string v1))) e1 sigma
    | Expr_Bool b -> (Value2 (Bool b), sigma)
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
  | Assign (v, e) -> begin
    let (v1, sigma') = (eval_expr e sigma) in
    match v1 with
    | Value2 v1' -> Finished (update_env v v1' sigma') |> add_default_label
    | Label2 l -> (l, Finished sigma')
  end
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
  | If (e, p1, p2) -> begin
    let (v1, sigma' ) = (eval_expr e sigma) in
    match v1 with
    | Value2 v1' ->
      if (value_to_bool v1') then
	Continue (p1, sigma') |> add_default_label
      else
	Continue (p2, sigma') |> add_default_label
    | Label2 l -> (l, Finished sigma')
  end
  | While (e, p1) -> begin
    let (v1, sigma' ) = (eval_expr e sigma) in
    match v1 with
    | Value2 v1' ->
      if (value_to_bool v1') then
	Continue (Seq (p1, p), sigma') |> add_default_label
      else
	Finished sigma' |> add_default_label
    | Label2 l -> (l, Finished sigma')
  end
  | For (var, e1, e2, p1) -> begin
    let (x1, sigma1 ) = (eval_expr e1 sigma) in
    match x1 with
    | Label2 l -> (l, Finished sigma1)
    | Value2 v1 -> begin
      let (x2, sigma2 ) = (eval_expr e2 sigma1) in
      match x2 with
      | Label2 l -> (l, Finished sigma2)
      | Value2 v2 ->
	let sigma3 = update_env var v1 sigma1 in
	if (value_to_int v1) < (value_to_int v2) then
	  Continue (Seq (p1, For(var, Expr_PrePlus(var), Expr_Num (value_to_int v2), p1)), sigma3) |> add_default_label
	else if (value_to_int v1) > (value_to_int v2) then
	  Continue (Seq (p1, For(var, Expr_PreMinus(var), Expr_Num (value_to_int v2), p1)), sigma3) |> add_default_label
	else
	  Finished sigma3 |> add_default_label
    end
  end 
  | Print expr -> begin
    let (x1, sigma' ) = (eval_expr expr sigma) in
    match x1 with
    | Label2 l -> (l, Finished sigma')
    | Value2 v1 ->
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
    | Label2 l -> (l, Finished sigma')
    | Value2 (Prog p) -> begin
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
    | _ -> ((Label "eval_non_prog", Some (String "Eval can only be applied to Prog values!")), Finished sigma')
  end
  | Unsupported -> begin
    ((Label "unsupported_command", Some (String "This command is not supported yet!")), Finished sigma)
  end

(** Fermeture reflexive-transitive de [step] *)
let rec run (p, sigma) : ToyEnv.env =
  print_env sigma;
  let label, outcome = step (p, sigma) in
  "↓ " ^ (label |> string_of_label_indented 2) ^ "\n"
  |> print_string;
  let label_exception, label_print = label in
  match label_exception with
  | Tau -> begin
    match outcome with
    | Continue (p', sigma') -> run (p', sigma')
    | Finished sigma' -> print_env sigma'
    ;
    print_endline "Program stopped normally.";
    outcome |> env_of_outcome
  end
  | Label l -> begin
    let sigma' = outcome |> env_of_outcome in
    print_env sigma';
    print_endline ("Program stopped because of an uncaught exception: " ^ l);
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
  run (prog, ToyEnv.init_env) |> ignore

