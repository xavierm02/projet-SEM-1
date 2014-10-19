(** Fichiers de définitions auxiliaires *)
open ToyTypes

(** {2 Fonctions utiles pour la manipulations des valeurs TOY} *)

(** [print_value v] affiche la valeur [v] sur la sortie standard. *)
let print_value v =
  match v with
  | Int(n) -> print_int n

(** [int_to_value i] convertit l'entier [i] en valeur TOY. *)
let int_to_value n = Int(n)

(** [value_to_int v] convertit la valeur TOY [v] en entier. *)
let value_to_int v =
  match v with
  | Int(n) -> n

(** [value0] est la constante 0 de TOY *)
let value0 = int_to_value 0

(** [lift_binop op v w] relève l'opérateur [op] sur les valeurs TOY *)
let lift_binop op v w =
  match v, w with
  | Int(n), Int(m) -> Int(op n m)

let lift_binop_bool op v w =
  match v, w with
  | Int(n), Int(m) -> Int(if op n m then 1 else 0)

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



let (|>) x f = f x
let (%>) f g x = x |> f |> g
