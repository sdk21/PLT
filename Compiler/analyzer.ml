(* Semantic Analyzer 
	- Consumes abstract syntax tree from parser
	- Produces semantically Analyzed Syntax Tree for compiler
*)

open Ast
open Sast

(* Variable Declarations *)
type var_decl = { var_type : Ast.vtype;
                  var_name : string; }

(* Symbol Table *)
type symbol_table = { parent : symbol_table option;
                      mutable variables : var_decl list; 
                      int }

(* Environment *)
type environment = { scope : symbol_table;
                     formal_params : var_decl list option
                     return_type : Ast.vtype }


