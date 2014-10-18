(** Définition de l'interpréteur TOY *)
open ToyTypes
open ToyEnv
open Utils

(** [eval_expr e s] évalue l'expression [e] dans l'environnement [s] *)
let rec eval_expr expr (sigma: ToyEnv.env) : value * env =
  match expr with
    | Expr_Num (n) -> (Utils.int_to_value n, sigma)
    | Expr_Var v -> begin
	  match eval_env v sigma with
	    | Some x -> (x, sigma)
	    | None -> failwith "Uninitialized_Variable v0"
	 end
    | Expr_Plus (e1, e2) -> let (v1, sigma') = (eval_expr e1 sigma) in
                          let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop (+) v1 v2,  sigma'')
    | Expr_Minus (e1, e2) -> let (v1, sigma') = (eval_expr e1 sigma) in
                          let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop (-) v1 v2,  sigma'')                     
    | Expr_Mult (e1, e2) -> let (v1, sigma')=(eval_expr e1 sigma) in
                          let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop ( * ) v1 v2, sigma'')
    | Expr_Div (e1, e2) -> let (v1, sigma')=(eval_expr e1 sigma) in
                          let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop ( / ) v1 v2, sigma'')
    | Expr_Equal (e1, e2) -> let (v1, sigma')=(eval_expr e1 sigma) in
                           let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop_bool_int (=) v1 v2, sigma'')
    | Expr_And (e1, e2) -> let (v1, sigma')=(eval_expr e1 sigma) in
                           let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop_bool_bool (&&) v1 v2, sigma'')
    | Expr_Less (e1, e2) -> let (v1, sigma')=(eval_expr e1 sigma) in
                          let (v2, sigma'')=(eval_expr e2 sigma') in
                          (lift_binop_bool_int (<) v1 v2, sigma'')
    | Expr_PostPlus (v) -> begin
          match eval_env v sigma with
                | Some x ->  let x1 = int_to_value ((value_to_int x) + 1) in
                             (x, (update_env v x1 sigma))
                | None -> failwith "Uninitialized_Variable v1"
          end
    | Expr_PostMinus (v) -> begin
          match eval_env v sigma with
                | Some x ->  let x1 = int_to_value ((value_to_int x) - 1) in
                              (x, (update_env v x1 sigma))
                | None -> failwith "Uninitialized_Variable v2"
          end
    | Expr_PrePlus (v) -> begin
          match eval_env v sigma with
                | Some x ->  let x1 = int_to_value (( value_to_int x) + 1) in
                              (x1, (update_env v x1 sigma))
                | None -> failwith "Uninitialized_Variable v3"
          end   
    | Expr_PreMinus (v) -> begin
          match eval_env v sigma with
                | Some x ->  let x1 = int_to_value (( value_to_int x) - 1) in
                              (x1, (update_env v x1 sigma))
                | None -> failwith "Uninitialized_Variable v4"
          end
    | Expr_Eassign (v,e) -> let (v1, sigma') = eval_expr e sigma in
                            begin match eval_env v sigma with
                                  | Some x ->  (x, (update_env v v1 sigma'))
                                  | None -> failwith "Uninitialized_Variable v5"
                            end
    |_ -> raise Unsupported_Expression
    
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
                     Finished (update_env v v1 sigma')
  | Seq (p1, p2) -> begin
    match step (p1, sigma) with
      | Continue (p1', sigma') -> Continue (Seq (p1', p2), sigma')
      | Finished sigma' -> Continue (p2, sigma')
  end
  | If (e, p1, p2) -> 
    let (v1, sigma' ) = (eval_expr e sigma) in
    if (value_to_bool v1) then
      Continue (p1, sigma')
    else
	  Continue (p2, sigma')
  | While (e, p1) -> begin
    let (v1, sigma' ) = (eval_expr e sigma) in
    if (value_to_bool v1) then
      Continue (Seq (p1, p), sigma')
    else
	  Finished sigma'
  end
  | Print expr -> begin
    print_string "> ";
    let (v1, sigma' ) = (eval_expr expr sigma) in
    print_value v1;
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
