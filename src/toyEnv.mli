(** Environnements d'évaluation des programmes TOY *)
open ToyTypes

(** Type abstrait des environnements *)
type env

(** [eval_env x e] évalue la variable [x] dans l'environnement [e] *)
val eval_env : var -> env -> value option

(** [eval_env x v e] assigne la valeur [v] à la variable [x] dans
    l'environnement [e] *)
val update_env : var -> value -> env -> env

(** [init_env] est l'environnement initial *)
val init_env : env

(** [string_of_env e] construit une représentation lisible de l'environnement [e] *)
val string_of_env : env -> string

(** [string_of_env e] affiche une représentation lisible de l'environnement [e] *)
val print_env : env -> unit
