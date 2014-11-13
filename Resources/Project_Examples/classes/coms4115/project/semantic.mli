(* David Golub *)

open Ast
open Icode
open Labels
open Vars

exception InvalidMatrix
exception TypeMismatch
exception WrongDataType

val check_matrix : matrix -> vartable -> (int * iprog)
val check_expr : expr -> vartable -> (datatype * int * iprog)
val check_lvalue : lvalue -> vartable -> (datatype * int * iprog)
val check_stmt : stmt -> vartable -> labels -> iprog
val check_prgm : prgm -> vartable -> labels -> iprog