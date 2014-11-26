(* Compiler
    - Consumes semantically analyzed syntax tree from semantic analyzer
    - Produces compiled program
 *)

(*let rec eval = function 
    Lit_int(x) -> 1
    | Binop(x1, op, x2) -> 2
    | Mat(x) -> 3

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (Ast.astprint result)
*)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.expr Scanner.token lexbuf in
  print_string (Ast.string_of_expr prog)



(*type action = Ast | Sast  (* | Java | Debug *)

let _ =
  let action = (* if Array.length Sys.argv > 1 then *)
    List.assoc Sys.argv.(1) [ ("-a", Ast); ("-s", Sast); (*("-j", Java); ("-d", Debug);*)]
  (* else Java *) in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with 
  | Ast -> 
      print_string (Ast.string_of_program program)
  | Sast -> print_string (Ast.string_of_program program) (* Repeated just to remove error for now *)
  *)
