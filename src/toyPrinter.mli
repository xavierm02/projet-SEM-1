(** Pretty printer pour le langage TOY *)
open ToyTypes
open Printf

(** [string_of_var v] construit la chaîne représentant la variable [v] *)
val string_of_var : var -> string

(** [string_of_value v] construit la chaîne représentant la valeur [v] *)
val string_of_value : value -> string

(** [print_expr p] affiche l'expression [e] sur la sortie standard *)
val print_expr : expr -> unit

(** [print_prog p] affiche le program [p] sur la sortie standard *)
val print_prog : prog -> unit

