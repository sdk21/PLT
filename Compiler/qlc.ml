(* Compiler
    - Consumes semantically analyzed syntax tree from semantic analyzer
    - Produces compiled program
 *)

type action = Ast | Sast | Gen | Debug

let _ =
  let action =
    List.assoc Sys.argv.(1) [("-a", Ast); ("-s", Sast); ("-g", Gen); ("-d", Debug);]
  in
    let lexbuf = Lexing.from_channel stdin in
      let program = Parser.program Scanner.token lexbuf in
        match action with 
          Ast ->  print_string (Ast.string_of_program program)
          | Sast -> 
            let sprogram =
              Analyzer.check_program program
            in
              print_string (Sast.string_of_program sprogram)
          | Gen -> print_string "gen"
          | Debug -> print_string "debug"