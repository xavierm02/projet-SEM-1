(** Fichiers de définitions auxiliaires *)
open ToyTypes

(** {2 Fonctions utiles pour la manipulations des valeurs TOY} *)
let print_bool b = print_string (string_of_bool b)
(** [print_value v] affiche la valeur [v] sur la sortie standard. *)
let print_value v =
  match v with
  | Int(n) -> print_int n
  | Bool(b) -> print_bool b
  | String(s) -> print_string s
  | Prog(p) -> print_string "<program>"

let value_to_string = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | String s -> s
  | Prog p -> "<program>"

(** [string_of_value v] construit la chaîne représentant la valeur [v] *)
let string_of_value : value -> string =
  function
  | Int(i) -> string_of_int i
  | Bool(b) -> string_of_bool b
  | String s ->  "\"" ^ (String.escaped s) ^ "\""
  | Prog p -> "<program>"

let string_to_value s = String s

(** [int_to_value i] convertit l'entier [i] en valeur TOY. *)
let int_to_value n = Int(n)

(** [value_to_int v] convertit la valeur TOY [v] en entier. *)
let value_to_int v =
  match v with
  | Int(n) -> n
  | _ -> failwith"value_to_int: expected value parameter of type Bool"

let value_to_bool v =
  match v with
  | Bool(b) -> b
  | _ -> failwith"value_to_int: expected value parameter of type Int"


(** [value0] est la constante 0 de TOY *)
let value0 = int_to_value 0

let lift_unop_int op v =
  match v with
  | Int(n) -> Int (op n)
  | _ -> failwith "lift_unop: expected value parameter of type Int"

let lift_unop_bool op v =
  match v with
  | Bool(n) -> Bool (op n)
  | _ -> failwith "lift_unop: expected value parameter of type Int"

(** [lift_binop op v w] relève l'opérateur [op] sur les valeurs TOY *)
let lift_binop_int_int op v w =
  match v, w with
  | Int(n), Int(m) -> Int(op n m)
  | _ -> failwith"lift_binop: expected value parameters of type Int"

let lift_binop_int_bool op v w =
  match v, w with
  | Int(n), Int(m) -> Bool(op n m)
  | _ -> failwith"lift_binop_int_bool: expected value parameters of type Int" 

let lift_binop_bool_bool op v w =
  match v, w with
  | Bool(n), Bool(m) -> Bool(op n m)
  | _ -> failwith"lift_binop_bool_bool: expected value parameters of type Bool" 
 
(** {2 Fonctions d'entrée/sortie fichiers} *)

(**/**)
(*  Voir 
    http://stackoverflow.com/questions/11276985/emulating-try-with-finally-in-ocaml *)
let unwind ~protect f x =
  let module E = struct type 'a t = Left of 'a | Right of exn end in
  let res = try E.Left (f x) with e -> E.Right e in
  let () = protect x in
  match res with
  | E.Left  y -> y
  | E.Right e -> raise e

let with_input_channel inch f =
  unwind ~protect:close_in f inch

let with_output_channel otch f =
  unwind ~protect:close_out f otch

let with_input_file fname =
  with_input_channel (open_in fname)

let with_output_file fname =
  with_output_channel (open_out fname)
(**/**)

(** [parse filename] analyse le fichier [filename] et produit la
    représentation abstraite du programme qu’il contient *)
let parse source =
  with_input_file source
  (fun ch ->
    ToyParser.make_prog ToyLexer.make_token (Lexing.from_channel ch)
  )

let string_of_exception_label =
  function
  | Tau -> "τ"
  | Label l -> l

let string_of_print_label = function
  | None -> ""
  | Some s -> "> " ^ (value_to_string s)
  

let string_of_label ((exception_label, print_label) : label) =
  (string_of_exception_label exception_label) ^ " " ^ (string_of_print_label print_label)


let (|>) x f = f x
let (%>) f g x = x |> f |> g
