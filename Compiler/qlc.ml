(* Compiler
    - Consumes semantically analyzed syntax tree from semantic analyzer
    - Produces compiled program
 *)

open Ast

let rec eval = function 
    Lit_int(x) -> 1
    | Binop(x1, op, x2) -> 2
    | Mat(x) -> 3

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (Ast.astprint result)