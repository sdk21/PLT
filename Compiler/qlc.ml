(* Compiler
    - Consumes semantically analyzed syntax tree from semantic analyzer
    - Produces compiled program
 *)

type action = Ast | Sast | Gen | Debug

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
  let action =
    List.assoc Sys.argv.(1) [("-a", Ast); ("-s", Sast); ("-g", Gen); ("-d", Debug);]
  in
    let lexbuf = Lexing.from_channel stdin in
      let program = Parser.expr Scanner.token lexbuf in
        match action with 
          Ast ->  print_string (Ast.string_of_expr program)
          | Sast -> print_string (Ast.string_of_expr program)
          | Gen -> print_string (Ast.string_of_expr program)
          | Debug -> print_string (Ast.string_of_expr program)
