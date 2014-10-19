open ToyTypes
open Utils

module M = Map.Make(String)

type env = value M.t

let eval_env (Var x) (e: env) : value option =
  try Some (M.find x e)
  with Not_found -> None

let update_env (Var x) v e = M.add x v e

let init_env = M.empty

let string_of_env e =
  "{ " ^
  M.fold ToyPrinter.(fun k v s ->
    Printf.sprintf "%s ↦ %s; %s"
    (string_of_var (Var k))
    (string_of_value v)
  s) e "}"

let print_env e =
	e |> string_of_env |> print_string;
	print_newline ()
