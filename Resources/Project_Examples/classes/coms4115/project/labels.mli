(* David Golub *)

open Ast

type labels

val new_labels : unit -> labels
val add_label : labels -> int
val get_label_name : int -> string
