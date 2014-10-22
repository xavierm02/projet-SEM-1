(** Pretty printer pour le langage TOY *)
open ToyTypes
open Printf

val print_bool : bool -> unit
val print_value : value -> unit
val value_to_string : value -> string
val string_of_value : value -> string
val string_of_exception_label : label_exception -> string
val string_of_print_label : label_print -> string
val string_of_label : label -> string
val string_of_label_indented : int -> label -> string

(** [string_of_var v] construit la chaîne représentant la variable [v] *)
val string_of_var : var -> string

(** [print_expr p] affiche l'expression [e] sur la sortie standard *)
val print_expr : expr -> unit

(** [print_prog p] affiche le program [p] sur la sortie standard *)
val print_prog : prog -> unit

