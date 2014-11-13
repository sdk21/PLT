(* David Golub *)

open Ast

type vartable

exception VariableNotFound of string
exception DuplicateDefinition of string

val new_vartable : unit -> vartable
val check_variable : string -> vartable -> bool
val add_variable : string -> datatype -> vartable -> int
val add_temp : datatype -> vartable -> int
val find_variable : string -> vartable -> int
val get_variable_name : int -> vartable -> string
val get_variable_type : int -> vartable -> datatype
val gencpp_vartable : vartable -> string
