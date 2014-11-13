(* David Golub *)

open Ast

type variable = { name : string; typ : datatype }
type vartable = { mutable table : variable array; mutable next : int }

exception VariableNotFound of string
exception DuplicateDefinition of string

let new_vartable () = { table = [| |]; next = 0 }
let check_variable name vt =
    let rec helper n =
        if n < vt.next then
            if vt.table.(n).name = name then
                true
            else
                helper (n + 1)
        else
            false
     in helper 0
let add_variable name typ vt =
    if check_variable name vt then
        raise (DuplicateDefinition name)
    else
        let varno = vt.next
         in vt.table <- Array.append vt.table [| { name = name; typ = typ } |];
            vt.next <- varno + 1;
            varno
let add_temp typ vt =
    let name = "_temp" ^ string_of_int vt.next
    in add_variable name typ vt
let find_variable name vt =
    let rec helper n =
        if n < vt.next then
            if vt.table.(n).name = name then
                n
            else
                helper (n + 1)
        else
            raise (VariableNotFound name)
     in helper 0
let get_variable_name n vt =
    vt.table.(n).name
let get_variable_type n vt =
    vt.table.(n).typ
let gencpp_datatype typ =
    match typ with
        Scalar  -> "Scalar"
      | Matrix  -> "Matrix"
      | String  -> "String"
      | Boolean -> "Boolean"
let gencpp_vartable vt =
    let rec helper n = 
        if n < 0 then
            ""
        else
            "\t" ^ (gencpp_datatype vt.table.(n).typ) ^ " " ^ vt.table.(n).name ^ ";\n" ^ (helper (n - 1))
    in
        helper (vt.next - 1)
